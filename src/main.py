import sys

# profiling
from contextlib import contextmanager
import cProfile, pstats, io
from pstats import SortKey

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

@contextmanager
def profiled():
    pr = cProfile.Profile()
    pr.enable()
    yield
    pr.disable()
    s = io.StringIO()
    sortby = SortKey.CUMULATIVE
    ps = pstats.Stats(pr, stream=s).sort_stats(sortby)
    ps.print_stats()
    log.debug(s.getvalue())

if True: # show debug
    logging.basicConfig(stream=sys.stdout, level=logging.DEBUG)

log = logging.getLogger(__name__)

log.debug("---Tokenizer:")
tokenizer = Tokenizer(
"""
void main() is
    int x = 0
    while (x < 100000) do
        x = x + 1
    print(x)
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
        log.debug(instruction)
        instruction = instruction.next

log.debug("---Bytecode:")
bytecode_generator = ProgramBytecodeGenerator(ir_generator.entrypoints)
bytecode = bytecode_generator.compile()
global_symbol_indices = bytecode_generator.get_global_symbol_indices()
function_entrypoints = bytecode_generator.get_functions_entrypoint_mapping()

log.debug("---Interpreter:")
with profiled():
    interpreter = Interpreter(bytecode, global_symbol_indices, function_entrypoints)
    interpreter.run()