import sys
from tokenizer.tokenizer import Tokenizer
from tokenizer.tokens import *

from parser.parser import Parser
from parser.ast_visitor import AstDebugVisitor
from parser.symbol import TypeChecker

from bytecode.instruction_generator import InstructionGenerator
from bytecode.bytecode_generator import ProgramBytecodeGenerator

from interpreter.interpreter import Interpreter

# setup logging
import logging
import sys
logging.basicConfig(stream=sys.stdout, level=logging.DEBUG)



print("---Tokenizer:")
tokenizer = Tokenizer(
"""int fibonacci(int n) is
    n = 3
    return n
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

node = ir_generator.start

while node != None:
    print(node)
    node = node.next

print("---Bytecode:")
bytecode_generator = ProgramBytecodeGenerator(ir_generator.entrypoints)
bytecode = bytecode_generator.compile()
print(bytecode)

print("---Interpreter:")
interpreter = Interpreter(bytecode)
interpreter.run()