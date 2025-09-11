from bytecode.instructions import *

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

class Interpreter:
    def __init__(self, bytecode):
        self.bytecode = bytecode
        self.funcs = self._find_function_metadata(bytecode)

        self.call_stack: list[Frame] = []

    def run(self):
        arg = 15
        print(f"First arg: {arg}")
        frame = Frame(0)
        frame.value_stack.append(arg)
        self.call_stack.append(frame) # TODO find main function!

        while True:
            cf = self.call_stack[-1]
            instr = self.bytecode[cf.ip]; cf.ip += 1

            # header_size = 12 # don't worry about it - matches our logging better without ENTER statement...
            # print(f"{cf.ip - header_size} {InstructionType(instr)} ({','.join(str(value) for value in cf.value_stack)})")

            if instr == InstructionType.NOOP.value:
                pass
            elif instr == InstructionType.LOAD_CONST_INT.value:
                const_value = get_u32(self.bytecode, cf.ip); cf.ip += 4
                cf.value_stack.append(const_value)
            elif instr == InstructionType.LOAD_LOCAL_INT.value:
                local_index = get_u16(self.bytecode, cf.ip); cf.ip += 2
                const_value = cf.locals[local_index]
                cf.value_stack.append(const_value)
            elif instr == InstructionType.LOAD_FUNC.value:
                func_index = get_u16(self.bytecode, cf.ip); cf.ip += 2
                func_entrypoint = self.funcs[func_index].entrypoint
                cf.value_stack.append(func_entrypoint)
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
                arg_count = get_u8(self.bytecode, cf.ip); cf.ip += 1
                args = list(cf.value_stack.pop() for _ in range(arg_count))
                func_entrypoint = cf.value_stack.pop()

                new_frame = Frame(func_entrypoint)
                self.call_stack.append(new_frame)
                
                for arg in args:
                    new_frame.value_stack.append(arg)

            elif instr == InstructionType.RETURN.value:
                return_value = cf.value_stack.pop()
                self.call_stack.pop()

                if len(self.call_stack) == 0:
                    print("Main function returned!")
                    print(f"Return code: {return_value}")
                    break
                
                caller_frame = self.call_stack[-1]
                caller_frame.value_stack.append(return_value)
            else:
                raise Exception(f"Some instruction we don't recognize. {instr}")

    def _find_function_metadata(self, bytecode) -> dict[int, FunctionMetadata]:
        funcs = {}

        pointer = 0
        while pointer < len(bytecode):
            func_entrypoint = pointer
            assert bytecode[pointer] == InstructionType.ENTER.value; pointer += 1

            func_index = get_u16(bytecode, pointer); pointer += 2
            func_args = get_u16(bytecode, pointer); pointer += 2
            func_locals = get_u16(bytecode, pointer); pointer += 2
            func_length = get_u32(bytecode, pointer); pointer += 4
            pointer += func_length

            func_metadata = FunctionMetadata(func_index, func_entrypoint, func_args, func_locals)
            funcs[func_index] = func_metadata

        return funcs