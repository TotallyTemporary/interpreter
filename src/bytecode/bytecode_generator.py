from parser.symbol import Symbol
from bytecode.instructions import *
from bytecode.instruction_visitor import InstructionVisitor, DefaultInstructionVisitor
from bytecode.instruction_generator import Entrypoint

def to_bytes(value: int, bytes=4, signed=False):
    """
    Convert a value into bytes, like a 32-bit int to 4 bytes, etc.
    """
    return [byte for byte in value.to_bytes(bytes, byteorder='big', signed=signed)]

class ProgramBytecodeGenerator():
    def __init__(self, entrypoints: list[Entrypoint]):
        self.entrypoints = entrypoints
        self.func_placeholders: dict[int, Symbol] = {}
        self.bytecode: list[int] = []

    def compile(self):
        for index, entrypoint in enumerate(self.entrypoints):
            self._compile_func(entrypoint, index)
        self._do_func_index_substitution()
        return self.bytecode

    def _do_func_index_substitution(self):
        func_indices = self._get_func_indexes_dict()
        for bytecode_index, placeholder_symbol in self.func_placeholders.items():
            func_index = func_indices[placeholder_symbol]
            self.bytecode[bytecode_index:bytecode_index+2] = to_bytes(func_index, 2)

    def _get_func_indexes_dict(self) -> dict:
        symbol_dict = {}
        
        symbols = [entrypoint.symbol for entrypoint in self.entrypoints]
        for index, symbol in enumerate(symbols):
            symbol_dict[symbol] = index
        return symbol_dict

    def _compile_func(self, entrypoint: Entrypoint, func_index: int):
        # compile the function
        func_bytecode_generator = FunctionBytecodeGenerator(entrypoint)
        func_bytecode = func_bytecode_generator.create_bytecode()

        # add metadata for interpreter about our function
        self.bytecode.extend(to_bytes(InstructionType.ENTER.value, 1))      # header
        self.bytecode.extend(to_bytes(func_index, 2)) # function index (like func#0 or func#1)
        self.bytecode.extend(to_bytes(len(func_bytecode_generator.arg_symbols), 2))   # argument count
        self.bytecode.extend(to_bytes(len(func_bytecode_generator.local_symbols), 2)) # local count
        self.bytecode.extend(to_bytes(len(func_bytecode), 4))

        bytecode_start_index = len(self.bytecode)
        self.bytecode.extend(func_bytecode)

        # keep track of the function globals, they've not been substituted in yet.
        func_globals = {
            bytecode_start_index + index: glob
            for index, glob in func_bytecode_generator.func_placeholders.items()
        }
        self.func_placeholders.update(func_globals)

class FunctionBytecodeGenerator(InstructionVisitor):
    def __init__(self, entrypoint: Entrypoint):
        self.entrypoint = entrypoint

        self.local_symbols, self.arg_symbols = self._find_locals_and_args()
        self.symbol_to_index_dict = self._create_symbol_dict()
        self.bytecode: list[int] = []

        self.instruction_to_byte_index_dict: dict[Instruction, int] = {} # filled out as bytecode is generated
        self.jump_placeholders: dict[int, Instruction] = {}
        self.func_placeholders: dict[int, Symbol] = {}

    def create_bytecode(self):
        self.visit(self.entrypoint.body)
        self._fill_jump_placeholders()
        return self.bytecode

    def _fill_jump_placeholders(self):
        """
        If we need to jump, say, 10 bytes to an instruction, we won't have generated the bytecode for that yet.
        Instead, in the jump we write a placeholder for the jump offset and write down the instruction.
        Here, we resolve that jump.
        """
        for placeholder_byte_index, instr in self.jump_placeholders.items():
            instr_byte_index = self.instruction_to_byte_index_dict[instr]
            jump_arg_to_instr_diff = instr_byte_index - placeholder_byte_index
            jump_to_instr_diff = jump_arg_to_instr_diff - 2 # we want to increment from the jump instruction, not from the jump argument, of course.
            self.bytecode[placeholder_byte_index:placeholder_byte_index+2] = to_bytes(jump_to_instr_diff, 2, signed=True)

    def _find_locals_and_args(self):
        arg_symbols = self.entrypoint.params

        local_symbols = FunctionLocalVariableFinder(self.entrypoint).get_symbols()
        local_symbols -= set(arg_symbols)

        return local_symbols, arg_symbols

    def _create_symbol_dict(self):
        symbol_dict = {}

        all_symbols = self.arg_symbols + list(self.local_symbols)

        for index, symbol in enumerate(all_symbols):
            symbol_dict[symbol] = index
        return symbol_dict

    def _get_local_index(self, symbol: Symbol):
        return to_bytes(self.symbol_to_index_dict[symbol], 2)
    
    def _add_local_placeholder(self, instruction: Instruction):
        self.jump_placeholders[len(self.bytecode)] = instruction

    def _add_func_placeholder(self, symbol: Symbol):
        self.func_placeholders[len(self.bytecode)] = symbol

    def visit(self, instruction):
        self.instruction_to_byte_index_dict[instruction] = len(self.bytecode)
        super().visit(instruction)

    def _log(self, msg):
        print(f"{len(self.bytecode)}: {msg}")

    def visit_NoOp(self, instruction):
        self._log("NoOp")
        self.bytecode.extend(to_bytes(InstructionType.NOOP.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Label(self, instruction):
        self._log("Label")
        self.bytecode.extend(to_bytes(InstructionType.NOOP.value, 1))
        self.visit_if_exists(instruction.next)
    
    def visit_LoadConstInt(self, instruction):
        self._log("LoadConstInt")
        self.bytecode.extend(to_bytes(InstructionType.LOAD_CONST_INT.value, 1))
        self.bytecode.extend(to_bytes(instruction.value, 4))
        self.visit_if_exists(instruction.next)

    def visit_LoadLocalInt(self, instruction):
        self._log("LoadLocalInt")
        self.bytecode.extend(to_bytes(InstructionType.LOAD_LOCAL_INT.value, 1))
        self.bytecode.extend(self._get_local_index(instruction.var))
        self.visit_if_exists(instruction.next)

    def visit_LoadFunc(self, instruction: LoadFunc):
        self._log("LoadFunc")
        self.bytecode.extend(to_bytes(InstructionType.LOAD_FUNC.value, 1))
        self._add_func_placeholder(instruction.var)
        self.bytecode.extend(to_bytes(0, 2))
        self.visit_if_exists(instruction.next)

    def visit_StoreLocalInt(self, instruction):
        self._log("StoreLocalInt")
        self.bytecode.extend(to_bytes(InstructionType.STORE_LOCAL_INT.value, 1))
        self.bytecode.extend(self._get_local_index(instruction.var))
        self.visit_if_exists(instruction.next)

    def visit_Pop(self, instruction):
        self._log("Pop")
        self.bytecode.extend(to_bytes(InstructionType.POP.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Dup(self, instruction):
        self._log("Dup")
        self.bytecode.extend(to_bytes(InstructionType.DUP.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Swap(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.SWAP.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Add(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.ADD.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Sub(self, instruction):
        self._log("Sub")
        self.bytecode.extend(to_bytes(InstructionType.SUB.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Mul(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.MUL.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Div(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.DIV.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Equals(self, instruction):
        self._log("Equals")
        self.bytecode.extend(to_bytes(InstructionType.EQ.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_NotEquals(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.NEQ.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_LessThan(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.LT.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_LessThanEquals(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.LTE.value, 1))
        self.visit_if_exists(instruction.next)
    
    def visit_GreaterThan(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.GT.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_GreaterThanEquals(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.GTE.value, 1))
        self.visit_if_exists(instruction.next)

    def visit_Jump(self, instruction):
        self.bytecode.extend(to_bytes(InstructionType.JUMP.value, 1))

        self._add_local_placeholder(instruction.instruction)
        self.bytecode.extend(to_bytes(0, bytes=2, signed=True))

        self.visit_if_exists(instruction.next)

    def visit_JumpIfZero(self, instruction):
        self._log("Jumpz")
        self.bytecode.extend(to_bytes(InstructionType.JUMPZ.value, 1))

        self._add_local_placeholder(instruction.cond_instr)
        self.bytecode.extend(to_bytes(0, bytes=2, signed=True))

        self.visit_if_exists(instruction.next)

    def visit_JumpIfNotZero(self, instruction):
        self._log("Jumpnz")
        self.bytecode.extend(to_bytes(InstructionType.JUMPNZ.value, 1))

        self._add_local_placeholder(instruction.cond_instr)
        self.bytecode.extend(to_bytes(0, bytes=2, signed=True))

        self.visit_if_exists(instruction.next)

    def visit_CallFunc(self, instruction):
        self._log("Call")
        self.bytecode.extend(to_bytes(InstructionType.CALL.value, 1))
        self.bytecode.extend(to_bytes(instruction.arg_count, 1))
        self.visit_if_exists(instruction.next)

    def visit_Return(self, instruction):
        self._log("Return")
        self.bytecode.extend(to_bytes(InstructionType.RETURN.value, 1))
        self.visit_if_exists(instruction.next)

class FunctionLocalVariableFinder(DefaultInstructionVisitor):
    def __init__(self, entrypoint: Entrypoint):
        self.entrypoint = entrypoint
        self.symbols = set()

    def get_symbols(self):
        self.visit(self.entrypoint.body)
        return self.symbols

    def visit_LoadLocalInt(self, instruction):
        self.symbols.add(instruction.var)
        self.visit_if_exists(instruction.next)

    def visit_LoadFunc(self, instruction):
        # TODO local funcs?
        self.visit_if_exists(instruction.next)

    def visit_StoreLocalInt(self, instruction):
        self.symbols.add(instruction.var)
        self.visit_if_exists(instruction.next)
