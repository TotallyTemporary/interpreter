from parser.nodes import *
from parser.ast_visitor import AstVisitor, DefaultAstVisitor
from parser.symbol import ScopedSymbolTable, TypeCheckerException, Symbol, TypeSymbol, FuncSymbol, ClassSymbol, ClassFieldSymbol
from builtin.globals import VOID_TYPE, INT_TYPE, BOOL_TYPE, FUNC_TYPE, CLASS_TYPE, GLOBAL_SCOPE, Globals

class TypeChecker(AstVisitor):
    def __init__(self):
        self.global_scope = GLOBAL_SCOPE
        self._initialize_global_scope()

        # will be set for each function, a bit hacky, but it works
        self.local_scope = self.global_scope
        self.expected_return_value = VOID_TYPE
        self.any_return_hit = False
        self.local_variables = []
        self.inside_class = None

    def do_type_checking(self, root_node):
        self.visit(root_node) # does type checking, adds metadata to funcs, variables etc.
        CheckCodeAfterReturn().visit(root_node) # makes sure returns are always last in block
        CheckFuncsAlwaysReturn().visit(root_node) # makes sure all funcs return in all branches

    def _initialize_global_scope(self):
        for global_var in Globals.global_vars():
            self.global_scope.define(global_var)

    def visit_ExpressionStatementNode(self, node):
        self.visit(node.expr)

    def visit_BoolLiteralNode(self, node):
        return BOOL_TYPE

    def visit_IntLiteralNode(self, node):
        return INT_TYPE

    def visit_VariableNode(self, node):
        var_symbol = self.local_scope.lookup(node.name)
        node.symbol = var_symbol
        return var_symbol.type

    def visit_BinOpNode(self, node):
        type_left = self.visit(node.left)
        type_right = self.visit(node.right)
        op = node.op

        expected_operand_types: list[TypeSymbol]
        output_type: TypeSymbol
        match op:
            case '+' | '-' | '*' | '/' | '%':
                expected_operand_types = [INT_TYPE]
                output_type = INT_TYPE
            case '<' | '<=' | '>' | '>=' | '==' | '!=':
                expected_operand_types = [INT_TYPE, BOOL_TYPE]
                output_type = BOOL_TYPE
            case 'and' | 'or':
                expected_operand_types = [BOOL_TYPE]
                output_type = BOOL_TYPE
            case _:
                raise TypeCheckerException(f"Binary operator types are unexpected. {type_left=}, {type_right=}")

        if type_left not in expected_operand_types or type_right not in expected_operand_types:
            raise TypeCheckerException(f"Binop operands are wrong type {type_left=} {type_right=} {expected_operand_types=}")
        
        return output_type

    def visit_ReturnNode(self, node):
        type_symbol = self.visit(node.expr)
        if not type_symbol == self.expected_return_value:
            raise TypeCheckerException(f"Returning something way different to what we expected. {type_symbol=}, {self.expected_return_value=}")
        self.any_return_hit = True
        return VOID_TYPE

    def visit_DeclarationNode(self, node):
        if node.assign_expr is None:
            # bit of a hack, this is a class variable declaration, so let's do no type checking
            # TODO: make this into its own node anyway
            type_symbol = self.local_scope.lookup(node.type)
            var_symbol = ClassFieldSymbol(node.name, type_symbol, self.inside_class)
            self.local_scope.define(var_symbol)
            node.symbol = var_symbol
            return VOID_TYPE

        type_symbol = self.local_scope.lookup(node.type)
        expr_type_symbol = self.visit(node.assign_expr)

        if type_symbol != expr_type_symbol:
            raise TypeCheckerException(f"Declaration expression and variable type don't match: {type_symbol=}, {expr_type_symbol=}")
        
        var_symbol = Symbol(node.name, type_symbol)
        self.local_scope.define(var_symbol)
        node.symbol = var_symbol

        return VOID_TYPE

    def _get_lhs_symbol(self, lhs):
        """Get left hand side symbol for assignment operations."""

        if isinstance(lhs, MemberNode):
            symbol = self._get_lhs_symbol(lhs.left)
            return symbol.type.inside.lookup(lhs.attr)
        elif isinstance(lhs, VariableNode):
            return self.local_scope.lookup(lhs.name)
        else:
            raise TypeCheckerException(f"Is not valid left-hand-side for assignment: {type(lhs)}")

    def _fix_local_assignment(self, node: AssignmentNode):
        """Add `this` to any assignment with locals, this will help later on. This makes all `x = 2` into `this.x = 2`."""
        if self.inside_class is None:
            return

        # Find the first variable node that starts this chain
        deepest_var_node = node.left
        second_deepest_node = node
        while isinstance(deepest_var_node, MemberNode):
            second_deepest_node = deepest_var_node
            deepest_var_node = deepest_var_node.left
        if not isinstance(deepest_var_node, VariableNode):
            raise TypeCheckerException(f"Is not valid left-hand-side for assignment: {type(deepest_var_node)}")
        
        if deepest_var_node.name == "this":
            return
        
        second_deepest_node.left = MemberNode(VariableNode("this"), deepest_var_node.name)

    def visit_AssignmentNode(self, node: AssignmentNode):
        self._fix_local_assignment(node)
        var_symbol = self._get_lhs_symbol(node.left)
        self.visit(node.left)
        expr_type_symbol = self.visit(node.assign_expr)

        if var_symbol.type != expr_type_symbol:
            raise TypeCheckerException(f"Assignment expression and variable type don't match: {var_symbol=}, {expr_type_symbol=}")

        node.symbol = var_symbol
        return var_symbol.type

    def visit_MemberNode(self, node: MemberNode):
        left_type: ClassSymbol = self.visit(node.left)
        variable_node = left_type.inside.lookup(node.attr)
        node.symbol = variable_node
        return variable_node.type

    def visit_NewNode(self, new_node: NewNode):
        new_node.symbol = self.local_scope.lookup(new_node.name)           # type symbol
        new_node.constructor_symbol = new_node.symbol.inside.lookup("new") # constructor symbol

        expr_types = []
        for expr in new_node.expressions:
            type = self.visit(expr)
            expr_types.append(type)

        # Add `this` to the constructor params
        expr_types.insert(0, new_node.symbol)

        if not all(expr_type == arg_type for expr_type, arg_type in zip(expr_types, new_node.constructor_symbol.arg_symbols, strict=True)):
            raise TypeCheckerException(f"Types in this new-statement are not as expected. {expr_types} {new_node.constructor_symbol.arg_symbols}")


        return new_node.symbol

    def visit_BlockStatement(self, node):
        for child in node.statements:
            self.visit(child)
        return VOID_TYPE

    def visit_ParamNode(self, node):
        type_symbol = self.local_scope.lookup(node.type)
        
        var_symbol = Symbol(node.name, type_symbol)
        self.local_scope.define(var_symbol)
        node.symbol = var_symbol
        return VOID_TYPE

    def visit_IfNode(self, node):
        cond_type = self.visit(node.condition)
        if cond_type != BOOL_TYPE:
            raise TypeCheckerException(f"If condition must be a boolean. Was: {cond_type}")
        self.visit(node.body)
        return VOID_TYPE
    
    def visit_WhileNode(self, node):
        cond_type = self.visit(node.condition)
        if cond_type != BOOL_TYPE:
            raise TypeCheckerException(f"While condition must be a boolean. Was: {cond_type}")
        self.visit(node.body)
        return VOID_TYPE

    def visit_DoWhileNode(self, node):
        cond_type = self.visit(node.condition)
        if cond_type != BOOL_TYPE:
            raise TypeCheckerException(f"Do-While condition must be a boolean. Was: {cond_type}")
        self.visit(node.body)
        return VOID_TYPE

    def visit_FuncDeclNode(self, func_decl: FuncDeclNode):
        func_symbol = func_decl.symbol

        # Scoped inside
        self.local_scope = ScopedSymbolTable(func_decl.name, prev_scope := self.local_scope)
        self.expected_return_value = func_symbol.return_type
        self.any_return_hit = False

        for param in func_decl.params:
            self.visit(param)
            
        self.visit(func_decl.body)

        if not self.any_return_hit and self.expected_return_value != VOID_TYPE:
            raise TypeCheckerException(f"This function doesn't return but return type was expected.")
        
        # Exit inside function scope
        self.local_scope = prev_scope
        self.expected_return_value = VOID_TYPE
        self.any_return_hit = False

        return VOID_TYPE

    def visit_ClassDeclNode(self, node: ClassDeclNode):
        class_symbol: ClassSymbol = self.local_scope.lookup(node.name) # created during hoisting

        prev_scope = self.local_scope

        self.local_scope = class_symbol.inside
        self.inside_class = class_symbol

        # get fields
        field_symbols = []
        for var_decl in node.var_decls:
            self.visit(var_decl)
            field_symbols.append(var_decl.symbol)
        class_symbol.fields = field_symbols

        for func_node in node.funcs:
            self.visit(func_node)

        self.inside_class = None
        self.local_scope = prev_scope

    def visit_FuncCallNode(self, node: FuncCallNode):
        callee = node.left
        self.visit(callee)
        func_symbol = callee.symbol
        node.symbol = func_symbol

        expr_types = []
        for expr in node.expressions:
            type = self.visit(expr)
            expr_types.append(type)

        # this is a member method, let's add `this` to arguments
        if isinstance(callee, MemberNode):
            this_type = callee.left.symbol.type
            expr_types.insert(0, this_type)

        if len(expr_types) != len(func_symbol.arg_symbols):
            raise TypeCheckerException(f"Types in this call are the wrong length. {expr_types} {func_symbol.arg_symbols}")

        if not all(expr_type == arg_type for expr_type, arg_type in zip(expr_types, func_symbol.arg_symbols, strict=True)):
            pass
            # raise TypeCheckerException(f"Types in this call are not as expected. {expr_types} {func_symbol.arg_symbols}")

        return func_symbol.return_type

    def visit_ProgramNode(self, program):
        # hoist classes
        for class_node in program.classes:
            class_symbol = ClassSymbol(class_node.name, CLASS_TYPE)
            self.global_scope.define(class_symbol)
            class_node.symbol = class_symbol

        # hoist funcs
        for func_node in program.funcs:
            # get args
            args = [self.local_scope.lookup(param.type) for param in func_node.params]

            return_type = self.global_scope.lookup(func_node.type)
            func_symbol = FuncSymbol(func_node.name, FUNC_TYPE, return_type, arg_symbols=args)
            self.global_scope.define(func_symbol)
            func_node.symbol = func_symbol

        # hoist funcs inside classes
        for class_node in program.classes:
            class_symbol = self.global_scope.lookup(class_node.name)
            class_node.symbol = class_symbol
            class_symbol.inside = ScopedSymbolTable(class_node.name, parent=self.global_scope)

            for func_node in class_node.funcs:
                func_node.params.insert(0, ParamNode(class_node.name, "this")) # add `this` argument
                args = [self.local_scope.lookup(param.type) for param in func_node.params]
                
                return_type = class_symbol.inside.lookup(func_node.type)
                func_symbol = FuncSymbol(func_node.name, FUNC_TYPE, return_type, arg_symbols=args)
                class_symbol.inside.define(func_symbol)
                func_node.symbol = func_symbol

        for class_node in program.classes:
            self.visit(class_node)
        for func_node in program.funcs:
            self.visit(func_node)

class CheckFuncsAlwaysReturn(DefaultAstVisitor):
    def visit_ReturnNode(self, node):
        return True

    def visit_BlockStatement(self, node):
        if len(node.statements) == 0:
            return
        
        last = node.statements[-1]
        return self.visit(last)

    def visit_DoWhileNode(self, node: IfNode):
        self.visit(node.condition)
        body_returns = self.visit(node.body)
        return body_returns # body is run at least once with do-while.

    def visit_WhileNode(self, node: WhileNode):
        self.visit(node.condition)
        self.visit(node.body)
        return False

    def visit_IfNode(self, node: IfNode):
        self.visit(node.condition)
        self.visit(node.body)
        return False # TODO else

    def visit_FuncDeclNode(self, node):
        return self.visit(node.body)

    def visit_ClassDeclNode(self, node):
        for func in node.funcs:
            always_returns = self.visit(func)
            return_type = func.symbol.return_type
            if not always_returns and return_type != VOID_TYPE:
                raise TypeCheckerException(f"Function '{func.name}' doesn't return in all cases.")

    def visit_ProgramNode(self, node: ProgramNode):
        for class_decl in node.classes:
            self.visit(class_decl)

        for func in node.funcs:
            always_returns = self.visit(func)
            return_type = func.symbol.return_type
            if not always_returns and return_type != VOID_TYPE:
                raise TypeCheckerException(f"Function '{func.name}' doesn't return in all cases.")
            
class CheckCodeAfterReturn(DefaultAstVisitor):
    def visit_BlockStatement(self, node):
        for statement in node.statements:
            self.visit(statement)

        if len(node.statements) == 0:
            return
        
        for statement in node.statements[:-1]:
            if isinstance(statement, ReturnNode):
                raise TypeCheckerException(f"Unreachable code, return must be last instruction in a block.")