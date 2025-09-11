from enum import Enum

COUNTER = 0

InstructionType = Enum("InstructionType", [
    "NOOP",
    "LOAD_CONST_INT",
    "LOAD_LOCAL_INT", # 2
    "LOAD_FUNC",      # 3
    "STORE_LOCAL_INT",
    "POP",
    "DUP",
    "SWAP",
    "ADD",
    "SUB",
    "MUL",
    "DIV",
    "EQ", # 12
    "NEQ",
    "LT",
    "LTE",
    "GT",
    "GTE",
    "JUMP",
    "JUMPZ", # 19
    "JUMPNZ",
    "CALL",
    "RETURN", # 22
    "ENTER"
], start=0)

class Instruction:
    def __init__(self, next = None):
        global COUNTER
        self.next = next
        self.counter = COUNTER
        COUNTER += 1

    def __repr__(self):
        return f"{self.counter} (->{self.next.counter if self.next is not None else None}): {self.__class__.__name__}"

class NoOp(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Label(Instruction):
    def __init__(self, name: str, next=None):
        super().__init__(next)
        self.name = name

    def __repr__(self):
        return super().__repr__() + f"({self.name})"

class LoadConstInt(Instruction):
    def __init__(self, value, next=None):
        super().__init__(next)
        self.value = value

    def __repr__(self):
        return super().__repr__() + f"({self.value})"

class LoadLocalInt(Instruction):
    def __init__(self, var, next=None):
        super().__init__(next)
        self.var = var

    def __repr__(self):
        return super().__repr__() + f"({self.var})"

class LoadFunc(Instruction):
    def __init__(self, var, next=None):
        super().__init__(next)
        self.var = var

    def __repr__(self):
        return super().__repr__() + f"({self.var})"

class StoreLocalInt(Instruction):
    def __init__(self, var, next=None):
        super().__init__(next)
        self.var = var

    def __repr__(self):
        return super().__repr__() + f"({self.var})"

class Pop(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Dup(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Swap(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Add(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Sub(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Mul(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Div(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Equals(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class NotEquals(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class LessThan(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class LessThanEquals(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class GreaterThan(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class GreaterThanEquals(Instruction):
    def __init__(self, next=None):
        super().__init__(next)

class Jump(Instruction):
    def __init__(self, instruction=None, next=None):
        super().__init__(next)
        self.instruction = instruction

    def __repr__(self):
        return super().__repr__() + f"[{self.instruction}]"

class JumpIfZero(Instruction):
    def __init__(self, cond_instr=None, next=None):
        super().__init__(next)
        self.cond_instr = cond_instr

    def __repr__(self):
        return super().__repr__() + f"[{self.cond_instr}]"

class JumpIfNotZero(Instruction):
    def __init__(self, cond_instr=None, next=None):
        super().__init__(next)
        self.cond_instr = cond_instr

    def __repr__(self):
        return super().__repr__() + f"[{self.cond_instr}]"

class CallFunc(Instruction):
    def __init__(self, next=None, arg_count: int = 0):
        super().__init__(next)
        self.arg_count = arg_count

class Return(Instruction):
    def __init__(self, next=None):
        super().__init__(next)
