import logging
from contextlib import contextmanager
from parser.nodes import *

log = logging.getLogger(__name__)

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
        log.debug(f"{self.indent*" "}{text}")

    @contextmanager
    def indented(self):
        self.indent += 2    
        yield
        self.indent -= 2

    def visit_ExpressionStatementNode(self, node):
        self.indent_print(f"ExprStatement")
        with self.indented():
            self.visit(node.expr)

    def visit_BoolLiteralNode(self, node):
        self.indent_print(f"Bool({node.value})")

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
            if node.assign_expr is not None:
                self.visit(node.assign_expr)

    def visit_AssignmentNode(self, node):
        self.indent_print(f"Assign")
        with self.indented():
            self.visit(node.left)
            self.visit(node.assign_expr)

    def visit_MemberNode(self, node: MemberNode):
        self.indent_print(f"Member")
        with self.indented():
            self.visit(node.left)
            self.indent_print(node.attr)

    def visit_NewNode(self, node: NewNode):
        self.indent_print(f"New({node.name})")
        with self.indented():
            for expr in node.expressions:
                self.visit(expr)

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

    def visit_WhileNode(self, node):
        self.indent_print(f"While")
        with self.indented():
            self.visit(node.condition)
            self.visit(node.body)

    def visit_DoWhileNode(self, node):
        self.indent_print(f"DoWhile")
        with self.indented():
            self.visit(node.condition)
            self.visit(node.body)

    def visit_FuncDeclNode(self, node):
        self.indent_print(f"Func({node.type},{node.name})")
        with self.indented():
            for param in node.params:
                self.visit(param)
            self.visit(node.body)

    def visit_ClassDeclNode(self, node: ClassDeclNode):
        self.indent_print(f"Class({node.name})")
        with self.indented():
            for var_decl in node.var_decls:
                self.visit(var_decl)
            for func in node.funcs:
                self.visit(func)

    def visit_FuncCallNode(self, node):
        self.indent_print(f"Call")
        with self.indented():
            self.visit(node.left)
            for expr in node.expressions:
                self.visit(expr)

    def visit_ProgramNode(self, node):
        with self.indented():
            for class_decl in node.classes:
                self.visit(class_decl)
            for func in node.funcs:
                self.visit(func)


class DefaultAstVisitor(AstVisitor):
    def visit_ExpressionStatementNode(self, node):
        self.visit(node.expr)

    def visit_BoolLiteralNode(self, node):
        pass

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
        if node.assign_expr is not None:
            self.visit(node.assign_expr)

    def visit_AssignmentNode(self, node):
        self.visit(node.left)
        self.visit(node.assign_expr)

    def visit_MemberNode(self, node: MemberNode):
        self.visit(node.left)

    def visit_NewNode(self, node: NewNode):
        for expr in node.expressions:
            self.visit(expr)

    def visit_BlockStatement(self, node):
        for child in node.statements:
            self.visit(child)

    def visit_ParamNode(self, node):
        pass

    def visit_IfNode(self, node):
        self.visit(node.condition)
        self.visit(node.body)

    def visit_WhileNode(self, node):
        self.visit(node.condition)
        self.visit(node.body)

    def visit_DoWhileNode(self, node):
        self.visit(node.condition)
        self.visit(node.body)

    def visit_FuncDeclNode(self, node):
        for param in node.params:
            self.visit(param)
        self.visit(node.body)

    def visit_ClassDeclNode(self, node: ClassDeclNode):
        for var_decl in node.var_decls:
            self.visit(var_decl)
        for func in node.funcs:
            self.visit(func)

    def visit_FuncCallNode(self, node):
        self.visit(node.left)
        for expr in node.expressions:
            self.visit(expr)

    def visit_ProgramNode(self, node):
        for class_decl in node.classes:
            self.visit(class_decl)
        for func in node.funcs:
            self.visit(func)