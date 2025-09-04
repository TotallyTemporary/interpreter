
from tokenizer.tokenizer import Tokenizer
from tokenizer.tokens import *

from parser.parser import Parser
from parser.ast_visitor import AstDebugVisitor
from parser.symbol import TypeChecker

from bytecode.instruction_generator import InstructionGenerator
from bytecode.bytecode_generator import FunctionBytecodeGenerator

tokenizer = Tokenizer(
"""int fib(int n) is
    if n == 0 then return 0
    if n == 1 then return 1
    return fib(n - 2) + fib(n - 1)
""")

tokens = tokenizer.tokens()

for token in tokens:
    print(token)

parser = Parser(tokens)
program = parser.program()
AstDebugVisitor().visit(program)

TypeChecker().visit(program)

generator = InstructionGenerator()
generator.visit(program)

node = generator.start

while node != None:
    print(node)
    node = node.next

entrypoint = generator.entrypoints[0]
bytecode_generator = FunctionBytecodeGenerator(entrypoint)
bytecode_generator.visit(entrypoint.body)

for bytecode in bytecode_generator.bytecode:
    print(bytecode)