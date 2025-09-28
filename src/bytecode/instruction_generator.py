from parser.symbol import Symbol, ClassFieldSymbol
from parser.ast_visitor import AstVisitor
from parser.nodes import *
from bytecode.instructions import *
from builtin.globals import NativeFuncs, VOID_TYPE

class Entrypoint:
    def __init__(self, symbol: Symbol, body: Instruction, params: list[Symbol]):
        self.symbol = symbol
        self.body = body
        self.params = params

class InstructionGenerator(AstVisitor):
    def __init__(self):
        self.entrypoints = []

        self.start = self.head = Label("ProgramStart")

    def add_head(self, instruction):
        self.head.next = instruction
        self.head = instruction

    def break_head(self, instruction):
        """Don't link the previous instruction to this one."""
        self.head = instruction

    def visit_ExpressionStatementNode(self, node):
        self.visit(node.expr)
        self.add_head(Pop())

    def visit_BoolLiteralNode(self, node):
        value = 0 if node.value is False else 1
        self.add_head(LoadConstInt(value))

    def visit_IntLiteralNode(self, node):
        self.add_head(LoadConstInt(node.value))

    def visit_VariableNode(self, node):
        self.add_head(LoadLocalInt(node.symbol))

    def get_binop(self, op):
        match op:
            case "+":
                return Add()
            case '-': return Sub()
            case '*': return Mul()
            case '/': return Div()
            case '<': return LessThan()
            case '<=': return LessThanEquals()
            case '>': return GreaterThan()
            case '>=': return GreaterThanEquals()
            case '==': return Equals()
            case '!=': return NotEquals()
            case 'or': return Or()
            case 'and': return And()

    def visit_BinOpNode(self, node):
        self.visit(node.left)
        self.visit(node.right)
        instruction = self.get_binop(node.op)
        self.add_head(instruction)

    def visit_ReturnNode(self, node):
        self.visit(node.expr)
        self.add_head(Return())

    def visit_DeclarationNode(self, node):
        if node.assign_expr is not None:
            self.visit(node.assign_expr)
            self.add_head(StoreLocalInt(node.symbol))

    def _push_field_object(self, lhs: MemberNode | VariableNode):
        if isinstance(lhs, VariableNode):
            self.add_head(LoadLocalInt(lhs.symbol))
            return
        elif isinstance(lhs, MemberNode):
            self._push_field_object(lhs.left)
            self.add_head(GetField(lhs.symbol))
        else:
            raise Exception("No!")

    def visit_AssignmentNode(self, assign_node: AssignmentNode):
        is_field = isinstance(assign_node.symbol, ClassFieldSymbol)

        if is_field:
            lhs: MemberNode = assign_node.left
            self._push_field_object(lhs.left) # this.x, push `this` onto stack
            self.visit(assign_node.assign_expr)
            self.add_head(SetField(lhs.symbol))
            return

        self.visit(assign_node.assign_expr)
        self.add_head(StoreLocalInt(assign_node.symbol))

    def visit_MemberNode(self, node: MemberNode):
        self.visit(node.left)
        self.add_head(GetField(node.symbol))

    def visit_NewNode(self, node: NewNode):
        # Create a new object on the heap, store its pointer in the stack
        field_count = len(node.symbol.fields)
        self.add_head(NewObject(field_count))   

        self.add_head(Dup()) # one goes in as a param to the constructor, other is the return value

        # Add args
        for arg in node.expressions:
            self.visit(arg)

        # Call constructor
        constructor = node.constructor_symbol
        is_native = NativeFuncs.is_native_func(constructor)
        args_count = len(constructor.arg_symbols)
        if not is_native:
            self.add_head(CallFunc(constructor, arg_count=args_count))
        else:
            self.add_head(CallNativeFunc(constructor, arg_count=args_count))

        self.add_head(Pop()) # ignore the return value of the constructor, it's a void zero anyway.

    def visit_BlockStatement(self, node):
        for child in node.statements:
            self.visit(child)

    def visit_IfNode(self, node):
        self.visit(node.condition)

        self.add_head(jump := JumpIfZero())
        self.visit(node.body)
        self.add_head(after := Label(f"AfterIf"))

        jump.cond_instr = after

    def visit_WhileNode(self, node: WhileNode):
        """A while-loop.
        
        BeforeWhile:
            x + 2 == 0
            JumpIfZero AfterWhile
            print "Inside loop"
            Jump BeforeWhile
        AfterWhile:
        """
        self.add_head(before := Label(f"BeforeWhile"))
        self.visit(node.condition)

        self.add_head(jump_over := JumpIfZero())
        self.visit(node.body)
        self.add_head(jump_back := Jump())
        self.add_head(after := Label(f"AfterWhile"))

        jump_back.instruction = before
        jump_over.cond_instr = after

    def visit_DoWhileNode(self, node: WhileNode):
        """A do-while-loop.
        
        BeforeWhile:
            print "Inside loop"
            x + 2 == 0
            JumpIfNotZero BeforeWhile
        AfterWhile:
        """
        self.add_head(before := Label(f"BeforeWhile"))
        self.visit(node.body)
        self.visit(node.condition)

        self.add_head(jump_back := JumpIfNotZero())
        self.add_head(Label(f"AfterWhile"))

        jump_back.cond_instr = before

    def visit_FuncDeclNode(self, node):
        self.break_head(func_start := Label(f"FuncStart-{node.symbol.name}"))
        param_symbols = [param.symbol for param in node.params]
        entrypoint = Entrypoint(
            node.symbol, func_start, param_symbols,
        )
        self.entrypoints.append(entrypoint)

        self.add_head(Enter(arg_count=len(param_symbols))) # just metadata

        # Store args into locals
        for symbol in param_symbols:
            self.add_head(StoreLocalInt(symbol))

        self.visit(node.body)

        # void functions might not have return statements at all, but we still want to return something, even if it's just to pop it later.
        if node.symbol.return_type == VOID_TYPE:
            self.add_head(LoadConstInt(0))
            self.add_head(Return())

    def visit_FuncCallNode(self, node: FuncCallNode):
        is_native = NativeFuncs.is_native_func(node.symbol)
        is_member = isinstance(node.left, MemberNode)
        args_count = len(node.expressions)

        # If a member, then `this` is the first argument
        if is_member:
            self.visit(node.left.left)
            args_count += 1

        # Add rest args
        for arg in node.expressions:
            self.visit(arg)

        # Call
        if not is_native:
            self.add_head(CallFunc(node.symbol, arg_count=args_count))
        else:
            self.add_head(CallNativeFunc(node.symbol, arg_count=args_count))

    def visit_ClassDeclNode(self, node: ClassDeclNode):
        for var_decl in node.var_decls:
            self.visit(var_decl)
        for func in node.funcs:
            self.visit(func)

    def visit_ProgramNode(self, node):
        for class_decl in node.classes:
            self.visit(class_decl)
        for func in node.funcs:
            self.visit(func)
