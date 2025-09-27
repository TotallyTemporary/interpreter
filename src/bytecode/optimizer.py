import random
import logging
from bytecode.instructions import *
from bytecode.instruction_generator import Entrypoint
from bytecode.instruction_visitor import InstructionVisitor, DefaultInstructionVisitor

log = logging.getLogger(__name__)

class Optimizer:
    def __init__(self, entrypoints: Entrypoint):
        self.entrypoints = entrypoints

class FuncOptimizer:
    def __init__(self, entrypoint: Entrypoint):
        self.entrypoint = entrypoint

    def optimize(self):
        self.instructions = get_instruction_list(self.entrypoint.body)
        self.blocks = get_blocks(self.entrypoint.body)

        optimizations = [self.remove_nops] # ...TODO add more!

        rounds = 0
        while True:
            rounds += 1
            log.info(f"Optimizing round #{rounds}")
            # Optimize something
            optimization_func = random.choice(optimizations)
            optimization_func()

            # Only stop optimizing while we're not making changes
            new_instructions = get_instruction_list(self.entrypoint.body)
            if all(instr1 == instr2 for instr1, instr2 in zip(self.instructions, new_instructions)):
                break
            self.instructions = new_instructions
            self.blocks = get_blocks(self.entrypoint.body)
    
    def remove_nops(self):
        """Remove no-ops, code which does nothing. This includes literal no-ops, as well as code like load+pop."""
        # Only do nop-removal within a block (delineated by jumps and returns)
        for instructions in self.blocks:
            index = 0
            while index < len(instructions):
                instr = instructions[index]
                next_instr = instructions[index+1] if index+1 < len(instructions) else None
                index += 1
                
                if next_instr is not None:
                    # delete nops and labels
                    if isinstance(instr, NoOp) or isinstance(instr, Label):
                        log.debug(f"Eliding nop: replacing '%s' with '%s'", instr, next_instr)
                        self._replace_instr(instr, next_instr)

                    # delete assign into itself
                    if (
                        isinstance(instr, LoadLocalInt)
                        and isinstance(next_instr, StoreLocalInt)
                    ):
                        if instr.var == next_instr.var:
                            log.debug(f"Eliding assign into itself: Replacing '%s' with nop", instr)
                            self._replace_instr(instr, NoOp(next=next_instr.next))

                    # delete load into pop
                    if isinstance(next_instr, Pop) and any(
                        isinstance(instr, Class)
                        for Class in [LoadConstInt, LoadLocalInt, LoadGlobalInt]
                    ):
                        log.debug(f"Eliding load into pop: Replacing '%s' with nop", instr)
                        self._replace_instr(instr, NoOp(next=next_instr.next))

                    # Store then load -> dup then store
                    if isinstance(instr, LoadLocalInt) and isinstance(next_instr, StoreLocalInt):
                        if instr.var == next_instr.var:
                            log.debug(f"Eliding store then load: Replacing load with dup. '%s', '%s'", instr, next_instr)
                            self._replace_instr(instr, Dup(next=instr))
                            instr.next = next_instr.next

    def _replace_instr(self, instr: Instruction, replace_with: Instruction):
        for node in self.instructions:
            if getattr(node, "next", None) == instr:
                node.next = replace_with # pointer to next instruction
            if getattr(node, "instruction", None) == instr:
                node.instruction = replace_with # jump instruction target
            if getattr(node, "cond_instr", None) == instr:
                node.cond_instr = replace_with # conditional target

        if self.entrypoint.body == instr:
            self.entrypoint.body = replace_with # if this is the entrypoint, replace entrypoint as well

def get_blocks(root_instr: Instruction):
    """Split instructions into basic blocks, delineated by jumps, jump targets and returns."""
    jump_targets = []
    jump_instructions = []
    returns = []
    class JumpsVisitor(DefaultInstructionVisitor):
        def visit_Jump(self, instruction):
            jump_targets.append(instruction.instruction)
            jump_instructions.append(instruction)

        def visit_JumpIfZero(self, instruction):
            jump_targets.append(instruction.cond_instr)
            jump_instructions.append(instruction)

        def visit_JumpIfNotZero(self, instruction):
            jump_targets.append(instruction.cond_instr)
            jump_instructions.append(instruction)

        def visit_Return(self, instruction):
            returns.append(instruction)
    JumpsVisitor().visit(root_instr)

    starts = set(jump_targets)
    ends = set(jump_instructions) | set(returns)

    instructions_list = get_instruction_list(root_instr)

    blocks = []
    current_block = []
    for instr in instructions_list:
        if instr in starts and len(current_block) != 0:
            # Jump target - start a new block, this instruction will be the first one.
            blocks.append(current_block)
            current_block = []
            current_block.append(instr)
            continue
        elif instr in ends:
            # Jump, return, etc. Add this instruction and then start a new block.
            current_block.append(instr)
            blocks.append(current_block)
            current_block = []
            continue

        # otherwise...
        current_block.append(instr)

    if len(current_block) != 0:
        blocks.append(current_block)

    return blocks


def get_instruction_list(root_instr: Instruction):
    all_instructions = []
    class ListerVisitor(DefaultInstructionVisitor):
        def visit(self, node):
            all_instructions.append(node)
            super().visit(node)
    
    ListerVisitor().visit(root_instr)
    return all_instructions
