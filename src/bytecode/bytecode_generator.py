from parser.symbol import Symbol
from bytecode.instructions import *
from bytecode.instruction_visitor import InstructionVisitor
from bytecode.instruction_generator import Entrypoint

class ProgramBytecodeGenerator():
    def __init__(self, entrypoints: list[Entrypoint]):
        pass

class FunctionBytecodeGenerator(InstructionVisitor):
    def __init__(self, entrypoint: Entrypoint):
        self.entrypoint = entrypoint

        # jumps and var indices get resolved at the end
        self.bytecode: list[int | Symbol | Instruction] = []

    def visit_NoOp(self, node):
        self.visit_if_exists(node.next)
    
    def visit_LoadConstInt(self, node):
        self.bytecode.append(LOAD_CONST_INT)
        self.bytecode.append(node.value)
        self.visit_if_exists(node.next)

    def visit_LoadLocalInt(self, node):
        self.bytecode.append(LOAD_LOCAL_INT)
        self.bytecode.append(node.var)
        self.visit_if_exists(node.next)

    def visit_LoadFunc(self, node):
        self.bytecode.append(LOAD_FUNC)
        self.bytecode.append(node.var)
        self.visit_if_exists(node.next)

    def visit_StoreLocalInt(self, node):
        self.bytecode.append(STORE_LOCAL_INT)
        self.bytecode.append(node.var)
        self.visit_if_exists(node.next)

    def visit_Pop(self, node):
        self.bytecode.append(POP)
        self.visit_if_exists(node.next)

    def visit_Dup(self, node):
        self.bytecode.append(DUP)
        self.visit_if_exists(node.next)

    def visit_Swap(self, node):
        self.bytecode.append(SWAP)
        self.visit_if_exists(node.next)

    def visit_Add(self, node):
        self.bytecode.append(ADD)
        self.visit_if_exists(node.next)

    def visit_Sub(self, node):
        self.bytecode.append(SUB)
        self.visit_if_exists(node.next)

    def visit_Mul(self, node):
        self.bytecode.append(MUL)
        self.visit_if_exists(node.next)

    def visit_Div(self, node):
        self.bytecode.append(DIV)
        self.visit_if_exists(node.next)

    def visit_Equals(self, node):
        self.bytecode.append(EQ)
        self.visit_if_exists(node.next)

    def visit_NotEquals(self, node):
        self.bytecode.append(NEQ)
        self.visit_if_exists(node.next)

    def visit_LessThan(self, node):
        self.bytecode.append(LT)
        self.visit_if_exists(node.next)

    def visit_LessThanEquals(self, node):
        self.bytecode.append(LTE)
        self.visit_if_exists(node.next)
    
    def visit_GreaterThan(self, node):
        self.bytecode.append(GT)
        self.visit_if_exists(node.next)

    def visit_GreaterThanEquals(self, node):
        self.bytecode.append(GTE)
        self.visit_if_exists(node.next)

    def visit_Jump(self, node):
        self.bytecode.append(JUMP)
        self.bytecode.append(node.instruction)
        self.visit_if_exists(node.instruction)
        self.visit_if_exists(node.next)

    def visit_JumpIfZero(self, node):
        self.bytecode.append(JUMPZ)
        self.bytecode.append(node.cond_instr)
        self.visit_if_exists(node.cond_instr)
        self.visit_if_exists(node.next)

    def visit_JumpIfNotZero(self, node):
        self.bytecode.append(JUMPNZ)
        self.bytecode.append(node.cond_instr)
        self.visit_if_exists(node.cond_instr)
        self.visit_if_exists(node.next)

    def visit_CallFunc(self, node):
        self.bytecode.append(CALL)
        self.bytecode.append(node.arg_count)
        self.visit_if_exists(node.next)

    def visit_Return(self, node):
        self.bytecode.append(RETURN)
        self.visit_if_exists(node.next)
