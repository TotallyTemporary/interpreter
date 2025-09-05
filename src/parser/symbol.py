import logging
from parser.ast_visitor import AstVisitor
from parser.nodes import *

log = logging.getLogger(__name__)

class TypeCheckerException(Exception):
    def __init__(self, reason):
        super().__init__(f"Type checker error: {reason}")

class Symbol():
    def __init__(self, name: str, type: "Symbol" = None):
        self.name = name
        self.type = type

    def __repr__(self):
        return f"Symbol({self.name},{self.type})"
    
class FuncSymbol(Symbol):
    def __init__(self, name: str, type: "Symbol" = None, return_type: "Symbol" = None, arg_symbols: list["Symbol"] = []):
        super().__init__(name, type)
        self.return_type = return_type
        self.arg_symbols = arg_symbols

class TypeSymbol(Symbol):
    def __init__(self, name: str):
        super().__init__(name, None)

class SymbolTable():
    def __init__(self, name: str):
        self.name = name
        self.symbols = {}

    def define(self, symbol: Symbol):
        log.debug(f"Defining {symbol}")

        if symbol.name in self.symbols and self.symbols[symbol.name] is not symbol:
            raise TypeCheckerException(f"Double declaration of symbol {symbol.name}")
        self.symbols[symbol.name] = symbol

    def lookup(self, symbol_name: str, error_on_not_found=True) -> Symbol | None:
        log.debug(f"Looking up {symbol_name}")

        value = self.symbols.get(symbol_name, None)

        if value is None and error_on_not_found:
            raise TypeCheckerException(f"Symbol not found {symbol_name}")
        return value
    
class ScopedSymbolTable(SymbolTable):
    def __init__(self, name: str, parent: "ScopedSymbolTable" = None):
        super().__init__(name)
        self.parent = parent
        self.children = []
        if self.parent is not None:
            self.parent.add_child(self)

    def add_child(self, child: "ScopedSymbolTable"):
        self.children.append(child)

    def define(self, symbol: Symbol):
        super().define(symbol)

    def lookup(self, symbol_name: str, error_on_not_found=True) -> Symbol:
        value = super().lookup(symbol_name, error_on_not_found=False)
        if not value and self.parent is not None:
            value = self.parent.lookup(symbol_name)
        
        if value is None and error_on_not_found:
            raise TypeCheckerException(f"Symbol not found: {symbol_name}")
        return value

INT_TYPE = TypeSymbol("int")
BOOL_TYPE = TypeSymbol("bool")
FUNC_TYPE = TypeSymbol("__function")
VOID_TYPE = TypeSymbol("__void")

class TypeChecker(AstVisitor):
    def __init__(self):
        self.global_scope = ScopedSymbolTable("global")
        self.global_scope.define(INT_TYPE)
        self.global_scope.define(BOOL_TYPE)
        self.global_scope.define(FUNC_TYPE)
        self.global_scope.define(VOID_TYPE)

        # will be set for each function, a bit hacky, but it works
        self.local_scope = self.global_scope
        self.expected_return_value = VOID_TYPE
        self.any_return_hit = False
        self.local_variables = []

    def do_type_checking(self, root_node):
        self.visit(root_node)

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

        expected_operand_type: TypeSymbol
        output_type: TypeSymbol
        match op:
            case '+' | '-' | '*' | '/' | '%':
                expected_operand_type = INT_TYPE
                output_type = INT_TYPE
            case '<' | '<=' | '>' | '>=' | '==' | '!=':
                # TODO can't compare booleans :P
                expected_operand_type = INT_TYPE
                output_type = BOOL_TYPE
            case 'and' | 'or':
                expected_operand_type = BOOL_TYPE
                output_type = BOOL_TYPE
            case _:
                raise TypeCheckerException(f"Binary operator types are unexpected. {type_left=}, {type_right=}")

        if type_left != expected_operand_type or type_right != expected_operand_type:
            raise TypeCheckerException(f"Binop operands are wrong type {type_left=} {type_right=} {expected_operand_type=}")

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