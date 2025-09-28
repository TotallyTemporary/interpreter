import logging
from collections import defaultdict
from bytecode.instructions import *
from bytecode.stack_value import StackValue
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

        optimizations = [
            self.remove_nops,
            self.duplications,
            self.constant_folding,
        ]

        rounds = 0
        last_change_made_in_round = 0
        while True:
            rounds += 1
            log.info(f"Optimizing round #{rounds}")
            # Optimize something
            optimization_func = optimizations[rounds % len(optimizations)]
            optimization_func()

            new_instructions = get_instruction_list(self.entrypoint.body)

            # let's check if we've made any changes with our optimizations
            if not all(instr1 == instr2 for instr1, instr2 in zip(self.instructions, new_instructions)):
                last_change_made_in_round = rounds
            if rounds - last_change_made_in_round >= len(optimizations) * 2:
                break

            self.instructions = new_instructions
            self.blocks = get_blocks(self.entrypoint.body)

    def remove_nops(self):
        """Remove no-ops, code which does nothing. This includes literal no-ops, as well as code like load+pop."""
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

                    # load+store -> nop
                    if isinstance(instr, LoadLocalInt) and isinstance(next_instr, StoreLocalInt):
                        log.debug(f"Load+Store into Nop: Replacing '%s' and '%s' with nop", instr, next_instr)
                        self._replace_instr(instr, NoOp(next=instr.next))
                        self._replace_instr(next_instr, NoOp(next=next_instr.next))

                    # store+load -> dup+store
                    if isinstance(instr, StoreLocalInt) and isinstance(next_instr, LoadLocalInt):
                        log.debug(f"Store+Load into Dup+Store. '%s' and '%s'", instr, next_instr)
                        self._replace_instr(next_instr, NoOp(next=next_instr.next)) # remove load
                        self._replace_instr(instr, Dup(next=instr)) # add dup, next is store
                        instr.next = next_instr.next # store.next

    def duplications(self):
        for block in self.blocks:
            before_lst, after_lst = get_stack_values_for_block(block)

            # Double use of a load, add a dup to original load
            for index, (instr, stack_before, stack_after) in enumerate(zip(block, before_lst, after_lst)):
                allowed_instr_types = [LoadLocalInt, LoadGlobalInt]
                if not any(isinstance(instr, instr_type) for instr_type in allowed_instr_types):
                    continue

                # enumerate backwards to find closest place to dup from
                for prev_index, (prev_instr, prev_before, prev_after) in reversed(
                    list(enumerate(
                        zip(block[:index], before_lst[:index], after_lst[:index])
                    ))
                ):
                    if len(prev_after) > 0 and prev_after[-1] == stack_after[-1]:
                        # We'd really want to reuse this top-of-stack value instead of loading it again, can we do that?
                        if can_duplicate_top_value(
                            block[prev_index:index],
                            start_stack=prev_before,
                            end_stack=after_lst[index - 1],
                        ):
                            log.debug(
                                f"Double use of the same load, add a Dup. '%s' and '%s'",
                                prev_instr,
                                instr,
                            )
                            dup = Dup(next=prev_instr.next)
                            prev_instr.next = dup
                            self._replace_instr(instr, NoOp(next=instr.next))
                            return

            # Load with value already at top-of-stack, replace with a dup
            for index, (instr, stack_before, stack_after) in enumerate(zip(block, before_lst, after_lst)):
                allowed_instr_types = [LoadLocalInt, LoadGlobalInt]
                if not any(isinstance(instr, instr_type) for instr_type in allowed_instr_types):
                    continue

                if len(stack_after) >= 2:
                    if stack_after[-1] == stack_after[-2]:
                        log.debug(f"Double load, replace with a dup '%s'.", instr)
                        self._replace_instr(instr, Dup(next=instr.next))
                        return

    def constant_folding(self):
        for block in self.blocks:
            before_lst, after_lst = get_stack_values_for_block(block)

            # if we already know the result of a calculation at compile time, we can just load a constant.
            for instr, _, stack_after in zip(block, before_lst, after_lst):
                if len(stack_after) == 0 or stack_after[-1].is_known is False:
                    continue

                # [x, y] -> [res: int]
                if any(isinstance(instr, instr_type) for instr_type in [Add, Sub, Mul, Div]):
                    log.debug(f"Constant folded %s into a constant load.", instr)
                    self._replace_instr_lst(instr, [
                        Pop(),
                        Pop(),
                        LoadConstInt(value=stack_after[-1].key)
                    ])
                # [x, y] -> [res: bool]
                if any(isinstance(instr, instr_type) for instr_type in [Equals, NotEquals, LessThan, LessThanEquals, GreaterThan, GreaterThanEquals, Or, And]):
                    log.debug(f"Constant folded %s into a constant load.", instr)
                    self._replace_instr_lst(instr, [
                        Pop(),
                        Pop(),
                        LoadConstInt(value=stack_after[-1].key)
                    ])

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
        return replace_with

    def _replace_instr_lst(self, instr: Instruction, replace_with_lst: list[Instruction]):
        for node in self.instructions:
            if getattr(node, "next", None) == instr:
                node.next = replace_with_lst[0] # pointer to next instruction
            if getattr(node, "instruction", None) == instr:
                node.instruction = replace_with_lst[0] # jump instruction target
            if getattr(node, "cond_instr", None) == instr:
                node.cond_instr = replace_with_lst[0] # conditional target

        if self.entrypoint.body == instr:
            self.entrypoint.body = replace_with_lst[0] # if this is the entrypoint, replace entrypoint as well

        # chain these together for convenience
        for index in range(len(replace_with_lst)-1):
            replace_with_lst[index].next = replace_with_lst[index+1]
        replace_with_lst[-1].next = instr.next

        return replace_with_lst[-1]

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

def get_stack_values_for_block(block: list[Instruction], stack=None):
    """Get the expected stack values for each instruction in this block.
    Returns two arrays, before and after. These can be zipped with block to result with instruction, stack state before it, stack state after it."""
    globs = defaultdict(lambda: StackValue.unknown(expr="UnknownGlobal"))
    locals = defaultdict(lambda: StackValue.unknown(expr="UnknownLocal"))
    before = []
    after = []
    current = stack or []
    for instr in block:
        before.append(current.copy())

        match instr:
            case LoadConstInt() as i:
                current.append(StackValue.const(i.value))
            case LoadLocalInt() as i:
                current.append(locals[i.var])
            case LoadGlobalInt() as i:
                current.append(globs[i.var])
            case StoreLocalInt() as i:
                value = current.pop()
                locals[i.var] = value
            case NewObject() as i:
                current.append(StackValue.unknown(expr=f"NewObject{i.number_of_fields}"))
            case SetField() as i:
                current.pop()
                current.pop()
            case GetField() as i:
                current.pop()
                current.append(StackValue.unknown(expr=f"GetField({i.var.name})"))
            case Pop() as i:
                current.pop()
            case Dup() as i:
                value = current.pop()
                current.append(value)
                current.append(value)
            case Swap() as i:
                value1 = current.pop()
                value2 = current.pop()
                current.append(value1)
                current.append(value2)
            case Add() as i:
                current.append(StackValue.add(current.pop(), current.pop()))
            case Sub() as i:
                current.append(StackValue.sub(current.pop(), current.pop()))
            case Mul() as i:
                current.append(StackValue.mul(current.pop(), current.pop()))
            case Div() as i:
                current.append(StackValue.div(current.pop(), current.pop()))
            case Equals():
                current.append(StackValue.eq(current.pop(), current.pop()))
            case NotEquals():
                current.append(StackValue.neq(current.pop(), current.pop()))
            case LessThan():
                current.append(StackValue.lt(current.pop(), current.pop()))
            case LessThanEquals():
                current.append(StackValue.lte(current.pop(), current.pop()))
            case GreaterThan():
                current.append(StackValue.gt(current.pop(), current.pop()))
            case GreaterThanEquals():
                current.append(StackValue.gte(current.pop(), current.pop()))
            case Or():
                current.append(StackValue.oor(current.pop(), current.pop()))
            case And():
                current.append(StackValue.aand(current.pop(), current.pop()))
            case CallFunc() as i:
                for _ in range(i.arg_count):
                    current.pop()

                current.append(StackValue.unknown(expr=f"FuncCall"))
            case CallNativeFunc() as i:
                for _ in range(i.arg_count):
                    current.pop()
                current.append(StackValue.unknown(expr=f"NativeFuncCall"))
            case Return() as i:
                current.pop()
        after.append(current.copy())
    return before, after

def can_duplicate_top_value(instr_lst: list[Instruction], start_stack: list[StackValue], end_stack: list[StackValue]):
    """Returns True if you can duplicate the top value and it'll persist throughout these instructions."""
    try:
        # duplicate the top value
        marker = start_stack[-1]
        _, after = get_stack_values_for_block(instr_lst, stack=[*start_stack, marker])

        # if the resulting stack (after last instruction) is what we'd expect + marker, it's persisted
        if after[-1] == [*end_stack, marker]:
            return True
    except Exception:
        pass
    return False
