from parser.symbol import Symbol
from parser.ast_visitor import AstVisitor
from parser.nodes import *
from bytecode.instructions import *

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

    def visit_BinOpNode(self, node):
        self.visit(node.left)
        self.visit(node.right)
        instruction = self.get_binop(node.op)
        self.add_head(instruction)

    def visit_ReturnNode(self, node):
        self.visit(node.expr)
        self.add_head(Return())

    def visit_DeclarationNode(self, node):
        self.visit(node.assign_expr)
        self.add_head(StoreLocalInt(node.symbol))

    def visit_AssignmentNode(self, node):
        self.visit(node.assign_expr)
        self.add_head(StoreLocalInt(node.symbol))

    def visit_BlockStatement(self, node):
        for child in node.statements:
            self.visit(child)

    def visit_IfNode(self, node):
        self.visit(node.condition)

        self.add_head(jump := JumpIfZero())
        self.visit(node.body)
        self.add_head(after := Label(f"AfterIf"))

        jump.cond_instr = after

    def visit_FuncDeclNode(self, node):
        self.add_head(func_start := Label(f"FuncStart-{node.symbol.name}"))
        param_symbols = [param.symbol for param in node.params]
        entrypoint = Entrypoint(
            node.symbol, func_start, param_symbols,
        )
        self.entrypoints.append(entrypoint)

        self.visit(node.body)

    def visit_FuncCallNode(self, node: FuncCallNode):
        self.add_head(LoadFunc(node.symbol))
        for arg in node.expressions:
            self.visit(arg)
        self.add_head(CallFunc(arg_count=len(node.expressions)))

    def visit_ProgramNode(self, node):
        for func in node.funcs:
            self.visit(func)
