from parser.nodes import *
from parser.exceptions import *
from tokenizer.tokens import *

from typing import TypeVar

ExpectedToken = TypeVar("ExpectedToken", bound=Token)


class ParserFile:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.index = 0

    def current_token(self):
        if self.index >= len(self.tokens):
            raise UnexpectedEofException(self.tokens[-1])
        return self.tokens[self.index]
    
    def next_token(self):
        if self.index + 1 >= len(self.tokens):
            raise UnexpectedEofException(self.tokens[-1])
        return self.tokens[self.index+1]
    
    def next_next_token(self):
        if self.index + 2 >= len(self.tokens):
            raise UnexpectedEofException(self.tokens[-1])
        return self.tokens[self.index+2]

    def advance(self):
        self.index += 1


class Parser:
    def __init__(self, tokens: list[Token]):
        self.file = ParserFile(tokens)

    # program: (func_decl_list | class_decl)* EOF
    def program(self):
        funcs = []
        classes = []
        while not isinstance(self.file.current_token(), EofToken):
            if isinstance(self.file.current_token(), ClassToken):
                class_decl = self.class_decl()
                classes.append(class_decl)
            elif isinstance(self.file.current_token(), SymbolToken):
                func = self.func_decl()
                funcs.append(func)

        return ProgramNode(funcs, classes)

    # class_decl: CLASS SYMBOL IS func_decl_list
    def class_decl(self) -> ClassDeclNode:
        self._expect(ClassToken)
        class_name = self._expect(NonKwSymbolToken).chars
        self._expect(IsToken)

        func_decls = []
        var_decls = []

        # TODO one-line non-indented classes with nop or something
        self._expect(IndentToken)

        # Either var decl or func decl
        # int x = 0...
        # int x, y...
        # int x
        # int x()
        while not isinstance(self.file.current_token(), DeIndentToken):
            if isinstance(self.file.next_next_token(), LParenToken):
                func_decl = self.func_decl()
                func_decls.append(func_decl)
            else:
                var_decl = self.class_var_decl_statement()
                var_decls.append(var_decl)

        self._expect(DeIndentToken)

        return ClassDeclNode(class_name, func_decls, var_decls)

    # func_decl: SYMBOL SYMBOL LPAREN param_list RPAREN IS block
    def func_decl(self) -> FuncDeclNode:
        type_token = self._expect(NonKwSymbolToken)
        name_token = self._expect([NonKwSymbolToken, NewToken]) # 'new' can be name of constructor function
        self._expect(LParenToken)
        params = self.param_list()
        self._expect(RParenToken)
        self._expect(IsToken)
        body = self.block_statement()

        return FuncDeclNode(
            type_token.chars,
            name_token.chars,
            params,
            body,
        )

    # param_list: (SYMBOL SYMBOL)? (COMMA SYMBOL SYMBOL)*
    def param_list(self) -> list[ParamNode]:
        def parse_param():
            type_token = self._expect(NonKwSymbolToken)
            name_token = self._expect(NonKwSymbolToken)
            param = ParamNode(
                type_token.chars, name_token.chars
            )
            return param

        params = []

        # first param
        if isinstance(self.file.current_token(), NonKwSymbolToken):
            params.append(parse_param())

        # subsequent params should have comma
        while isinstance(self.file.current_token(), CommaToken):
            self._expect(CommaToken)
            params.append(parse_param())

        return params

    # block: statement | (INDENT statement* DEINDENT)
    def block_statement(self) -> BlockStatement:
        if not isinstance(self.file.current_token(), IndentToken):
            # single-line block statement
            statement = self.statement()
            return BlockStatement([statement])

        statements = []
        self._expect(IndentToken)
        while not isinstance(self.file.current_token(), DeIndentToken):
            statement = self.statement()
            statements.append(statement)
        self._expect(DeIndentToken)
        return BlockStatement(statements)

    # statement: expr | if_statement | return_statement | while_statement | do_while_statement | assign_statement | decl_statement | expr
    def statement(self) -> StatementNode:
        current_token = self.file.current_token()
        next_token = self.file.next_token()
        if isinstance(current_token, ReturnToken):
            return self.return_statement()
        elif isinstance(current_token, IfToken):
            return self.if_statement()
        elif isinstance(current_token, WhileToken):
            return self.while_statement()
        elif isinstance(current_token, DoToken):
            return self.do_while_statement()
        elif isinstance(current_token, NonKwSymbolToken) and isinstance(next_token, NonKwSymbolToken):
            return self.decl_statement()
        elif isinstance(current_token, NopToken):
            self._expect(NopToken)
            return BlockStatement([])
        else:
            # bit of a hack, but we can't look far enough in tokens to determine whether this is an expression or assignment
            # example: a.b.c.d.e = 0 vs. a.b.c.d.e
            left = self.expr()
            if isinstance(self.file.current_token(), AssignToken):
                return self.assign_statement(left)
            return ExpressionStatementNode(left)

    # assign_statement: SYMBOL EQUALS expr
    def assign_statement(self, left: ExpressionNode) -> AssignmentNode:
        self._expect(AssignToken)
        expr = self.expr()
        return AssignmentNode(left, expr)

    # decl_statement: SYMBOL SYMBOL EQUALS expr
    def decl_statement(self) -> DeclarationNode:
        type = self._expect(NonKwSymbolToken).chars
        name = self._expect(NonKwSymbolToken).chars
        self._expect(AssignToken)
        expr = self.expr()
        return DeclarationNode(type, name, expr)
    
    # TODO default class vars?
    # class_var_decl_statement: SYMBOL SYMBOL
    def class_var_decl_statement(self) -> DeclarationNode:
        type = self._expect(NonKwSymbolToken).chars
        name = self._expect(NonKwSymbolToken).chars
        return DeclarationNode(type, name, None)

    # while_statement: while expr do block_statement
    def while_statement(self) -> WhileNode:
        self._expect(WhileToken)
        cond = self.expr()
        self._expect(DoToken)
        body = self.block_statement()
        return WhileNode(cond, body)
    
    # do_while_statement: do block_statement while expr
    def do_while_statement(self) -> DoWhileNode:
        self._expect(DoToken)
        body = self.block_statement()
        self._expect(WhileToken)
        cond = self.expr()
        return DoWhileNode(cond, body)

    # if_statement: if expr then block_statement
    def if_statement(self) -> IfNode:
        self._expect(IfToken)
        cond = self.expr()
        self._expect(ThenToken)
        body = self.block_statement()
        return IfNode(cond, body)

    # return_statement: RETURN expr
    def return_statement(self) -> ReturnNode:
        self._expect(ReturnToken)
        return_expr = self.expr()
        return ReturnNode(return_expr)

    # expr: disjunction
    def expr(self):
        return self.disjunction()

    # disjunction: conjunction (OR conjunction)*
    def disjunction(self):
        left = self.conjunction()

        while isinstance(self.file.current_token(), OrToken):
            op = self._expect(OrToken).chars
            right = self.conjunction()
            left = BinOpNode(left, op, right)
        return left

    # conjunction: inversion (AND inversion)*
    def conjunction(self):
        left = self.inversion()

        while isinstance(self.file.current_token(), AndToken):
            op = self._expect(AndToken).chars
            right = self.inversion()
            left = BinOpNode(left, op, right)
        return left

    # TODO unary
    # inversion: NOT* comparison
    def inversion(self):
        return self.comparison()

    # comparison: sum ((LT | LTE | GT | GTE | EQ | NEQ) sum)*
    def comparison(self) -> ExpressionNode:
        left = self.sum()
        possible_ops = [LteToken, LtToken, GteToken, GtToken, EqualsToken, NotEqualsToken]
        
        while any([isinstance(self.file.current_token(), op) for op in possible_ops]):
            op = self._expect(possible_ops).chars
            right = self.sum()
            left = BinOpNode(left, op, right)
        return left

    # sum: term ((PLUS | MINUS) term)*
    def sum(self) -> ExpressionNode:
        left = self.term()

        while isinstance(self.file.current_token(), PlusToken) or isinstance(
            self.file.current_token(), MinusToken
        ):
            op = self._expect([PlusToken, MinusToken]).chars
            right = self.factor()
            left = BinOpNode(left, op, right)
        return left
    
    # TODO modulo
    # term: factor ((STAR | SLASH) factor)*
    def term(self) -> ExpressionNode:
        left = self.factor()

        while isinstance(self.file.current_token(), StarToken) or isinstance(
            self.file.current_token(), SlashToken
        ):
            op = self._expect([StarToken, SlashToken]).chars
            right = self.factor()
            left = BinOpNode(left, op, right)
        return left

    # factor: primary
    def factor(self) -> ExpressionNode:
        return self.primary()

    # primary: atom ((LPAREN args RPAREN) | (DOT SYMBOL))*
    def primary(self):
        left = self.atom()

        possible_tokens = [LParenToken, DotToken]
        while any(isinstance(self.file.current_token(), type) for type in possible_tokens):
            if isinstance(self.file.current_token(), LParenToken):
                self._expect(LParenToken)
                args = self.arg_list()
                self._expect(RParenToken)
                left = FuncCallNode(left, args)
            elif isinstance(self.file.current_token(), DotToken):
                self._expect(DotToken)
                attr = self._expect(NonKwSymbolToken).chars
                left = MemberNode(left, attr)

        return left

    # TODO strings, floats
    # atom: (NEW SYMBOL LPAREN arg_list RPAREN) | (LPAREN expr RPAREN) | SYMBOL | TRUE | FALSE | INTEGER
    def atom(self):
        current_token = self.file.current_token()
        match current_token:
            case NewToken() as token:
                self._expect(NewToken)
                name = self._expect(NonKwSymbolToken).chars
                self._expect(LParenToken)
                args = self.arg_list()
                self._expect(RParenToken)
                return NewNode(name, args)
            case LParenToken() as token:
                self._expect(LParenToken)
                expr = self.expr()
                self._expect(RParenToken)
                return expr
            case IntLiteralToken() as token:
                self._expect(IntLiteralToken)
                return IntLiteralNode(token.value)
            case NonKwSymbolToken() as token:
                name = self._expect(NonKwSymbolToken).chars
                return VariableNode(name)
            case TrueToken():
                self._expect(TrueToken)
                return BoolLiteralNode(True)
            case FalseToken():
                self._expect(FalseToken)
                return BoolLiteralNode(False)
            case _: raise UnexpectedTokenException(current_token, [IntLiteralToken, NonKwSymbolToken])

    # arg_list: expr? (COMMA expr)*
    def arg_list(self) -> list[ExpressionNode]:
        exprs = []

        if not isinstance(self.file.current_token(), RParenToken):
            expr = self.expr()
            exprs.append(expr)
        
        while isinstance(self.file.current_token(), CommaToken):
            self._expect(CommaToken)
            expr = self.expr()
            exprs.append(expr)
        
        return exprs

    def _expect(
        self,
        token_classes: type[ExpectedToken] | list[type[ExpectedToken]],
        do_advance=True,
    ) -> ExpectedToken:
        if not isinstance(token_classes, list):
            token_classes = [token_classes]

        current_token = self.file.current_token()

        for potential_class in token_classes:
            if isinstance(current_token, potential_class):
                break
        else:
            raise UnexpectedTokenException(current_token, token_classes)
        if do_advance:
            self.file.advance()
        return current_token
