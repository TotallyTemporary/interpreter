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
    
class DefaultInstructionVisitor(InstructionVisitor):
    def visit_NoOp(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Label(self, instruction):
        self.visit_if_exists(instruction.next)
    
    def visit_LoadConstInt(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_LoadLocalInt(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_LoadGlobalInt(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_StoreLocalInt(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Pop(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Dup(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Swap(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Add(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Sub(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Mul(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Div(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Equals(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_NotEquals(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_LessThan(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_LessThanEquals(self, instruction):
        self.visit_if_exists(instruction.next)
    
    def visit_GreaterThan(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_GreaterThanEquals(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Or(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_And(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Jump(self, instruction):
        # self.visit_if_exists(instruction.instruction)
        self.visit_if_exists(instruction.next)

    def visit_JumpIfZero(self, instruction):
        # self.visit_if_exists(instruction.cond_instr)
        self.visit_if_exists(instruction.next)

    def visit_JumpIfNotZero(self, instruction):
        # self.visit_if_exists(instruction.cond_instr)
        self.visit_if_exists(instruction.next)

    def visit_CallFunc(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_CallNativeFunc(self, instruction):
        self.visit_if_exists(instruction.next)

    def visit_Return(self, instruction):
        self.visit_if_exists(instruction.next)