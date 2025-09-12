from parser.nodes import *
from parser.ast_visitor import AstVisitor, DefaultAstVisitor
from parser.symbol import ScopedSymbolTable, TypeCheckerException, Symbol, TypeSymbol, FuncSymbol
from builtin.globals import VOID_TYPE, INT_TYPE, BOOL_TYPE, FUNC_TYPE, Globals

class TypeChecker(AstVisitor):
    def __init__(self):
        self.global_scope = ScopedSymbolTable("global")
        self._initialize_global_scope()

        # will be set for each function, a bit hacky, but it works
        self.local_scope = self.global_scope
        self.expected_return_value = VOID_TYPE
        self.any_return_hit = False
        self.local_variables = []

    def do_type_checking(self, root_node):
        self.visit(root_node) # does type checking, adds metadata to funcs, variables etc.
        CheckCodeAfterReturn().visit(root_node) # makes sure returns are always last in block
        CheckFuncsAlwaysReturn().visit(root_node) # makes sure all funcs return in all branches

    def _initialize_global_scope(self):
        self.global_scope.define(INT_TYPE)
        self.global_scope.define(BOOL_TYPE)
        self.global_scope.define(FUNC_TYPE)
        self.global_scope.define(VOID_TYPE)
        for global_var in Globals.global_vars():
            self.global_scope.define(global_var)

    def visit_ExpressionStatementNode(self, node):
        self.visit(node.expr)

    def visit_BoolLiteralNode(self, node):
        return self.global_scope.lookup("bool")

    def visit_IntLiteralNode(self, node):
        return self.global_scope.lookup("int")

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
        type_symbol = self.local_scope.lookup(node.type)
        expr_type_symbol = self.visit(node.assign_expr)

        if type_symbol != expr_type_symbol:
            raise TypeCheckerException(f"Declaration expression and variable type don't match: {type_symbol=}, {expr_type_symbol=}")
        
        var_symbol = Symbol(node.name, type_symbol)
        self.local_scope.define(var_symbol)
        node.symbol = var_symbol

        return VOID_TYPE

    def visit_AssignmentNode(self, node):
        var_symbol = self.local_scope.lookup(node.name)
        expr_type_symbol = self.visit(node.assign_expr)

        if var_symbol.type != expr_type_symbol:
            raise TypeCheckerException(f"Assignment expression and variable type don't match: {var_symbol=}, {expr_type_symbol=}")

        node.symbol = var_symbol
        return var_symbol.type

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

    def visit_FuncDeclNode(self, node: FuncDeclNode):
        return_type = self.local_scope.lookup(node.type)
        func_symbol = FuncSymbol(node.name, FUNC_TYPE, return_type)
        self.local_scope.define(func_symbol)
        node.symbol = func_symbol

        # Scoped inside
        self.local_scope = ScopedSymbolTable(node.name, prev_scope := self.local_scope)
        self.expected_return_value = return_type
        self.any_return_hit = False

        for param in node.params:
            self.visit(param)

        func_symbol.arg_symbols = [param.symbol.type for param in node.params]
        self.visit(node.body)

        if not self.any_return_hit and self.expected_return_value != VOID_TYPE:
            raise TypeCheckerException(f"This function doesn't return but return type was expected.")
        
        # Exit inside function scope
        self.local_scope = prev_scope
        self.expected_return_value = VOID_TYPE
        self.any_return_hit = False

        return VOID_TYPE

    def visit_FuncCallNode(self, node):
        func_symbol = self.local_scope.lookup(node.name)
        node.symbol = func_symbol

        passed_arguments_count = len(node.expressions)
        expected_arguments = len(func_symbol.arg_symbols)

        if passed_arguments_count != expected_arguments:
            raise TypeCheckerException(f"Wrong number of arguments. {passed_arguments_count=} {expected_arguments=}")

        expr_types = []
        for expr in node.expressions:
            type = self.visit(expr)
            expr_types.append(type)

        if not all(expr_type == arg_type for expr_type, arg_type in zip(expr_types, func_symbol.arg_symbols)):
            raise TypeCheckerException(f"Types in this call are not as expected. {expr_types} {func_symbol.arg_symbols}")

        return func_symbol.return_type

    def visit_ProgramNode(self, node):
        for func in node.funcs:
            self.visit(func)

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

    def visit_ProgramNode(self, node: ProgramNode):
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