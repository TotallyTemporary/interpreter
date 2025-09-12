class AstNode:
    pass

class ExpressionNode(AstNode):
    pass

class StatementNode(AstNode):
    pass

class ExpressionStatementNode(StatementNode):
    """An expression can be a statement, but we will need to pop the resulting value."""
    def __init__(self, expr: ExpressionNode):
        self.expr = expr

class BoolLiteralNode(ExpressionNode):
    def __init__(self, value: bool):
        self.value = value

class IntLiteralNode(ExpressionNode):
    def __init__(self, value: int):
        self.value = value

class VariableNode(ExpressionNode):
    def __init__(self, name: str):
        self.name = name

class BinOpNode(ExpressionNode):
    def __init__(self, left: ExpressionNode, op: str, right: ExpressionNode):
        self.left = left
        self.op = op
        self.right = right

class ReturnNode(StatementNode):
    def __init__(self, expr: ExpressionNode):
        self.expr = expr

class DeclarationNode(StatementNode):
    def __init__(self, type: str, name: str, assign_expr: ExpressionNode):
        self.type = type
        self.name = name
        self.assign_expr = assign_expr

class AssignmentNode(StatementNode):
    def __init__(self, name: str, assign_expr: ExpressionNode):
        self.name = name
        self.assign_expr = assign_expr

class BlockStatement(StatementNode):
    def __init__(self, statements: list[StatementNode]):
        self.statements = statements

class IfNode(StatementNode):
    def __init__(self, condition: ExpressionNode, body: BlockStatement):
        self.condition = condition
        self.body = body

class WhileNode(StatementNode):
    def __init__(self, condition: ExpressionNode, body: BlockStatement):
        self.condition = condition
        self.body = body

class DoWhileNode(StatementNode):
    def __init__(self, condition: ExpressionNode, body: BlockStatement):
        self.condition = condition
        self.body = body

class FuncCallNode(ExpressionNode):
    def __init__(self, name: str, expressions: list[ExpressionNode]):
        self.name = name
        self.expressions = expressions

class ParamNode(AstNode):
    def __init__(self, type: str, name: str):
        self.type = type
        self.name = name

class FuncDeclNode(AstNode):
    def __init__(self, type: str, name: str, params: list[ParamNode], body: StatementNode):
        self.type = type
        self.name = name
        self.params = params
        self.body = body

class ProgramNode(AstNode):
    def __init__(self, funcs: FuncDeclNode):
        self.funcs = funcs