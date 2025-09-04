from contextlib import contextmanager

class InstructionVisitorException(Exception):
    pass

class InstructionVisitorMethodNotFoundException(InstructionVisitorException):
    def __init__(self, node_type):
        super().__init__(f"Couldn't find visitor visit method for node type: {node_type}")

class InstructionVisitor():

    def visit(self, node):
        method_name = "visit_" + type(node).__name__
        method = getattr(self, method_name, self._generic_visit)
        return method(node)
    
    def visit_if_exists(self, node):
        if node != None:
            self.visit(node)
        else:
            pass
    
    def _generic_visit(self, node):
        raise InstructionVisitorMethodNotFoundException(type(node).__name__)