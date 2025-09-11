from contextlib import contextmanager

class VisitorException(Exception):
    pass

class VisitorMethodNotFoundException(VisitorException):
    def __init__(self, node_type):
        super().__init__(f"Couldn't find visitor visit method for node type: {node_type}")

class AstVisitor():

    def visit(self, node):
        method_name = "visit_" + type(node).__name__
        method = getattr(self, method_name, self._generic_visit)
        return method(node)
    
    def _generic_visit(self, node):
        raise VisitorMethodNotFoundException(type(node).__name__)
    
class AstDebugVisitor(AstVisitor):
    def __init__(self):
        self.indent = 0

    def indent_print(self, text):
        print(f"{self.indent*" "}{text}")

    @contextmanager
    def indented(self):
        self.indent += 2    
        yield
        self.indent -= 2

    def visit_IntLiteralNode(self, node):
        self.indent_print(f"Integer({node.value})")

    def visit_VariableNode(self, node):
        self.indent_print(f"Var({node.name})")

    def visit_BinOpNode(self, node):
        self.indent_print(f"BinOp({node.op})")
        with self.indented():
            self.visit(node.left)
            self.visit(node.right)

    def visit_ReturnNode(self, node):
        self.indent_print(f"Return")
        with self.indented():
            self.visit(node.expr)

    def visit_DeclarationNode(self, node):
        self.indent_print(f"Decl({node.type},{node.name})")
        with self.indented():
            self.visit(node.assign_expr)

    def visit_AssignmentNode(self, node):
        self.indent_print(f"Decl({node.name})")
        with self.indented():
            self.visit(node.assign_expr)

    def visit_BlockStatement(self, node):
        self.indent_print(f"Block")
        with self.indented():
            for child in node.statements:
                self.visit(child)

    def visit_ParamNode(self, node):
        self.indent_print(f"Param({node.type},{node.name})")

    def visit_IfNode(self, node):
        self.indent_print(f"If")
        with self.indented():
            self.visit(node.condition)
            self.visit(node.body)

    def visit_FuncDeclNode(self, node):
        self.indent_print(f"Func({node.type},{node.name})")
        with self.indented():
            for param in node.params:
                self.visit(param)
            self.visit(node.body)

    def visit_FuncCallNode(self, node):
        self.indent_print(f"Call({node.name})")
        with self.indented():
            for expr in node.expressions:
                self.visit(expr)

    def visit_ProgramNode(self, node):
        with self.indented():
            for func in node.funcs:
                self.visit(func)


class DefaultAstVisitor(AstVisitor):
    def visit_IntLiteralNode(self, node):
        pass

    def visit_VariableNode(self, node):
        pass

    def visit_BinOpNode(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_ReturnNode(self, node):
        self.visit(node.expr)

    def visit_DeclarationNode(self, node):
        self.visit(node.assign_expr)

    def visit_AssignmentNode(self, node):
        self.visit(node.assign_expr)

    def visit_BlockStatement(self, node):
        for child in node.statements:
            self.visit(child)

    def visit_ParamNode(self, node):
        pass

    def visit_IfNode(self, node):
        self.visit(node.condition)
        self.visit(node.body)

    def visit_FuncDeclNode(self, node):
        for param in node.params:
            self.visit(param)
        self.visit(node.body)

    def visit_FuncCallNode(self, node):
        for expr in node.expressions:
            self.visit(expr)

    def visit_ProgramNode(self, node):
        for func in node.funcs:
            self.visit(func)