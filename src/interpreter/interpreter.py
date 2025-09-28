import logging

from bytecode.instructions import *
from parser.symbol import Symbol
from builtin.globals import NativeFuncs, FUNC_TYPE

log = logging.getLogger(__name__)

def get_u32(data: list[int], pointer: int, signed=False) -> int:
    return int.from_bytes(data[pointer:pointer+4], signed=signed)

def get_u16(data: list[int], pointer: int, signed=False) -> int:
    return int.from_bytes(data[pointer:pointer+2], signed=signed)

def get_u8(data: list[int], pointer: int) -> int:
    return data[pointer]

class FunctionMetadata:
    def __init__(self, index, entrypoint, args_count, locals_count):
        self.index = index
        self.entrypoint = entrypoint
        self.args_count = args_count
        self.locals_count = locals_count

class Frame:
    def __init__(self, ip):
        self.ip = ip
        self.locals = None # don't worry, the ENTER instruction will fill these up for us. we don't as a caller yet know how many locals a func has.
        self.value_stack = []

class HeapObject:
    def __init__(self, number_of_fields: int):
        self.fields = [None]*number_of_fields

class Interpreter:
    def __init__(self, bytecode, global_symbol_indices: dict[Symbol, int], function_entrypoints: dict[Symbol, int]):
        self.bytecode = bytecode
        self.global_symbol_indices = global_symbol_indices
        self.function_entrypoints = function_entrypoints

        self._natives_dict = NativeFuncs.construct_native_funcs()

        self.globals = [None]*len(global_symbol_indices)
        self.natives = list(self._natives_dict.values())
        self.heap: list[HeapObject] = [None]*10
        self.call_stack: list[Frame] = []
        self._fill_globals()

    def _find_main_function(self):
        for func_symbol, global_index in self.global_symbol_indices.items():
            if func_symbol.name == "main" and func_symbol.type == FUNC_TYPE:
                return self.globals[global_index]
        raise Exception("No main function!")

    def _fill_globals(self):
        """Introduce values for global variables."""
        
        # functions are globals. their entrypoint is stored.
        for func_symbol, entrypoint in self.function_entrypoints.items():
            index = self.global_symbol_indices.get(func_symbol)

            # TODO, function that's never used will not have a spot in the globals array, check for this and remove entrypoint entirely
            if index is None:
                continue

            self.globals[index] = entrypoint

        # native functions are also globals, their index into the native array is stored.
        for native_index, native_symbol in enumerate(self._natives_dict.keys()):
            global_index = self.global_symbol_indices.get(native_symbol)

            # TODO, function that's never used will not have a spot in the globals array, check for this and remove entrypoint entirely
            if global_index is None:
                continue

            self.globals[global_index] = native_index

    def run(self):
        args = []
        log.debug(f"Args: {args}")

        main_func_ip = self._find_main_function()
        frame = Frame(main_func_ip)
        frame.value_stack.extend(reversed(args))
        self.call_stack.append(frame)

        while True:
            cf = self.call_stack[-1]
            instr = self.bytecode[cf.ip]; cf.ip += 1

            # log.debug(f"{cf.ip} {InstructionType(instr)} ({','.join(str(value) for value in cf.value_stack)})")

            if instr == InstructionType.NOOP.value:
                pass
            elif instr == InstructionType.LOAD_CONST_INT.value:
                const_value = get_u32(self.bytecode, cf.ip); cf.ip += 4
                cf.value_stack.append(const_value)
            elif instr == InstructionType.LOAD_LOCAL_INT.value:
                local_index = get_u16(self.bytecode, cf.ip); cf.ip += 2
                const_value = cf.locals[local_index]
                cf.value_stack.append(const_value)
            elif instr == InstructionType.LOAD_GLOBAL_INT.value:
                global_index = get_u16(self.bytecode, cf.ip); cf.ip += 2
                global_value = self.globals[global_index]
                cf.value_stack.append(global_value)
            elif instr == InstructionType.STORE_LOCAL_INT.value:
                local_index = get_u16(self.bytecode, cf.ip); cf.ip += 2
                value = cf.value_stack.pop()
                cf.locals[local_index] = value
            elif instr == InstructionType.POP.value:
                cf.value_stack.pop()
            elif instr == InstructionType.DUP.value:
                value = cf.value_stack.pop()
                cf.value_stack.append(value)
                cf.value_stack.append(value)
            elif instr == InstructionType.SWAP.value:
                value1 = cf.value_stack.pop()
                value2 = cf.value_stack.pop()
                cf.value_stack.append(value1)
                cf.value_stack.append(value2)
            elif instr == InstructionType.ADD.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = left + right
                cf.value_stack.append(result)
            elif instr == InstructionType.SUB.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = left - right
                cf.value_stack.append(result)
            elif instr == InstructionType.MUL.value:
                raise Exception("Not yet implemented")
            elif instr == InstructionType.DIV.value:
                raise Exception("Not yet implemented")
            elif instr == InstructionType.EQ.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left == right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.NEQ.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left != right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.LT.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left < right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.LTE.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left <= right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.GT.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left > right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.GTE.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left >= right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.AND.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left and right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.OR.value:
                right = cf.value_stack.pop()
                left = cf.value_stack.pop()
                result = 1 if left or right else 0
                cf.value_stack.append(result)
            elif instr == InstructionType.JUMP.value:
                dst = get_u16(self.bytecode, cf.ip, signed=True); cf.ip += 2
                cf.ip += dst
            elif instr == InstructionType.JUMPZ.value:
                dst = get_u16(self.bytecode, cf.ip, signed=True); cf.ip += 2
                value = cf.value_stack.pop()
                if value == 0:
                    cf.ip += dst
            elif instr == InstructionType.JUMPNZ.value:
                dst = get_u16(self.bytecode, cf.ip, signed=True); cf.ip += 2
                value = cf.value_stack.pop()
                if value != 0:
                    cf.ip += dst
            elif instr == InstructionType.ENTER.value:
                # we are a new function, handle args and locals
                _ = get_u16(self.bytecode, cf.ip); cf.ip += 2
                func_args = get_u16(self.bytecode, cf.ip); cf.ip += 2
                func_locals = get_u16(self.bytecode, cf.ip); cf.ip += 2
                _ = get_u32(self.bytecode, cf.ip); cf.ip += 4

                # reserve space for args and locals
                cf.locals = [None] * (func_args + func_locals)

                # take our args off the stack and into locals list
                for index in range(func_args):
                    cf.locals[index] = cf.value_stack.pop()
            elif instr == InstructionType.CALL.value:
                global_index = get_u16(self.bytecode, cf.ip); cf.ip += 2
                arg_count = get_u8(self.bytecode, cf.ip); cf.ip += 1
                args = list(cf.value_stack.pop() for _ in range(arg_count))
                func_entrypoint = self.globals[global_index]

                new_frame = Frame(func_entrypoint)
                self.call_stack.append(new_frame)
                
                for arg in args:
                    new_frame.value_stack.append(arg)
            elif instr == InstructionType.CALL_NATIVE.value:
                global_index = get_u16(self.bytecode, cf.ip); cf.ip += 2
                arg_count = get_u8(self.bytecode, cf.ip); cf.ip += 1
                args = list(cf.value_stack.pop() for _ in range(arg_count))
                args.reverse()

                native_index = self.globals[global_index]
                func = self.natives[native_index]
                return_value = func(*args)
                cf.value_stack.append(return_value)
            elif instr == InstructionType.RETURN.value:
                return_value = cf.value_stack.pop()
                self.call_stack.pop()

                if len(self.call_stack) == 0:
                    log.debug("Main function returned!")
                    # log.debug(f"Return value: {return_value}")

                    # log.debug("Heap: ")
                    # for idx, obj in enumerate(self.heap):
                    #     log.debug("%s %s", idx, getattr(obj, "fields", None))
                    break
                
                caller_frame = self.call_stack[-1]
                caller_frame.value_stack.append(return_value)
            elif instr == InstructionType.NEWOBJECT.value:
                field_count = get_u8(self.bytecode, cf.ip); cf.ip += 1
                heap_object = HeapObject(field_count)
                index = len(self.heap)
                self.heap.append(heap_object)
                cf.value_stack.append(index)
            elif instr == InstructionType.GETFIELD.value:
                var_index = get_u8(self.bytecode, cf.ip); cf.ip += 1
                object_index = cf.value_stack.pop()
                value = self.heap[object_index].fields[var_index]
                cf.value_stack.append(value)
            elif instr == InstructionType.SETFIELD.value:
                var_index = get_u8(self.bytecode, cf.ip); cf.ip += 1
                value = cf.value_stack.pop()
                object_index = cf.value_stack.pop()
                self.heap[object_index].fields[var_index] = value

            else:
                raise Exception(f"Some instruction we don't recognize. {instr}")
