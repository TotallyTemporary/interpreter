import sys
from tokenizer.tokenizer import Tokenizer
from tokenizer.tokens import *

from parser.parser import Parser
from parser.ast_visitor import AstDebugVisitor
from parser.type_checker import TypeChecker

from bytecode.instruction_generator import InstructionGenerator
from bytecode.bytecode_generator import ProgramBytecodeGenerator

from interpreter.interpreter import Interpreter

# setup logging
import logging
import sys
logging.basicConfig(stream=sys.stdout, level=logging.DEBUG)



print("---Tokenizer:")
tokenizer = Tokenizer(
"""
int fib(int n) is
    if n == 0 then return 0
    if n == 1 then return 1
    return fib(n - 1) + fib(n - 2)

void main() is
    int result = fib(15)
    print(result)
""")

tokens = tokenizer.tokens()

for token in tokens:
    print(token)

print("---Parser:")
parser = Parser(tokens)
program = parser.program()
AstDebugVisitor().visit(program)

print("---Type checker:")
type_checker = TypeChecker()
type_checker.do_type_checking(program)

print("---IR:")
ir_generator = InstructionGenerator()
ir_generator.visit(program)

instruction = ir_generator.start

while instruction != None:
    print(instruction)
    instruction = instruction.next

print("---Bytecode:")
bytecode_generator = ProgramBytecodeGenerator(ir_generator.entrypoints)
bytecode = bytecode_generator.compile()
global_symbol_indices = bytecode_generator.get_global_symbol_indices()
function_entrypoints = bytecode_generator.get_functions_entrypoint_mapping()

print("---Interpreter:")
interpreter = Interpreter(bytecode, global_symbol_indices, function_entrypoints)
interpreter.run()