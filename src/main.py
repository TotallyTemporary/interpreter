import sys
from tokenizer.tokenizer import Tokenizer
from tokenizer.tokens import *

from parser.parser import Parser
from parser.ast_visitor import AstDebugVisitor
from parser.type_checker import TypeChecker

from bytecode.instruction_generator import InstructionGenerator
from bytecode.bytecode_generator import ProgramBytecodeGenerator
from bytecode.optimizer import FuncOptimizer

from interpreter.interpreter import Interpreter

# setup logging
import logging
import sys

if False: # show debug
    logging.basicConfig(stream=sys.stdout, level=logging.DEBUG)

log = logging.getLogger(__name__)

log.debug("---Tokenizer:")
tokenizer = Tokenizer(
"""
class Vector3i is
    int x
    int y
    int z

    void new(int x, int y, int z) is
        this.x = x
        this.y = y
        this.z = z

    Vector3i add(Vector3i other) is
        return new Vector3i(this.x + other.x, this.y + other.y, this.z + other.z)


void main() is
    Vector3i pos = new Vector3i(1, 0, 0)
    Vector3i anew = pos.add(new Vector3i(0, 1, 1))
    print(pos.x)
    print(pos.y)
    print(pos.z)
    print(anew.x)
    print(anew.y)
    print(anew.z)
""")

tokens = tokenizer.tokens()

for token in tokens:
    log.debug(token)

log.debug("---Parser:")
parser = Parser(tokens)
program = parser.program()

log.debug("---Type checker:")
type_checker = TypeChecker()
type_checker.do_type_checking(program)

AstDebugVisitor().visit(program)

log.debug("---IR:")
ir_generator = InstructionGenerator()
ir_generator.visit(program)

for entrypoint in ir_generator.entrypoints:
    log.debug(f"Showing function '{entrypoint.symbol.name}'")
    instruction = entrypoint.body
    while instruction != None:
        log.debug(instruction)
        instruction = instruction.next

for entrypoint in ir_generator.entrypoints:
    FuncOptimizer(entrypoint).optimize()

for entrypoint in ir_generator.entrypoints:
    log.debug(f"Showing optimized function '{entrypoint.symbol.name}'")
    instruction = entrypoint.body
    while instruction != None:
        print(instruction)
        instruction = instruction.next

log.debug("---Bytecode:")
bytecode_generator = ProgramBytecodeGenerator(ir_generator.entrypoints)
bytecode = bytecode_generator.compile()
global_symbol_indices = bytecode_generator.get_global_symbol_indices()
function_entrypoints = bytecode_generator.get_functions_entrypoint_mapping()

log.debug("---Interpreter:")
interpreter = Interpreter(bytecode, global_symbol_indices, function_entrypoints)
interpreter.run()