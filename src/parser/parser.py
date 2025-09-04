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

    def advance(self):
        self.index += 1


class Parser:
    def __init__(self, tokens: list[Token]):
        self.file = ParserFile(tokens)

    # program: func_decl* EOF
    def program(self):
        funcs = []
        while not isinstance(self.file.current_token(), EofToken):
            func = self.func_decl()
            funcs.append(func)
        return ProgramNode(funcs)

    # func_decl: SYMBOL SYMBOL LPAREN param_list RPAREN IS block
    def func_decl(self):
        type_token = self._expect(NonKwSymbolToken)
        name_token = self._expect(NonKwSymbolToken)
        self._expect(LParenToken)
        params = self.param_list()
        self._expect(RParenToken)
        self._expect(IsToken)
        body = self.block_statement()

        return FuncDeclNode(
            type_token.symbol_string,
            name_token.symbol_string,
            params,
            body,
        )

    # param_list: (SYMBOL SYMBOL)? (COMMA SYMBOL SYMBOL)*
    def param_list(self) -> list[ParamNode]:
        def parse_param():
            type_token = self._expect(NonKwSymbolToken)
            name_token = self._expect(NonKwSymbolToken)
            param = ParamNode(
                type_token.symbol_string, name_token.symbol_string
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

    # statement: expr | if_statement | return_statement
    def statement(self) -> StatementNode:
        current_token = self.file.current_token()
        if isinstance(current_token, ReturnToken):
            return self.return_statement()
        elif isinstance(current_token, IfToken):
            return self.if_statement()
        else:
            raise UnexpectedTokenException(current_token, [ReturnToken, IfToken])

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

    # TODO unary
    # factor: primary | LPAREN expr RPAREN
    def factor(self) -> ExpressionNode:
        if isinstance(self.file.current_token(), LParenToken):
            self._expect(LParenToken)
            expr = self.expr()
            self._expect(RParenToken)
            return expr

        return self.primary()

    # primary: atom
    def primary(self):
        return self.atom()

    # TODO strings, floats
    # atom: SYMBOL | INTEGER
    def atom(self):
        current_token = self.file.current_token()
        match current_token:
            case IntLiteralToken() as token:
                self._expect(IntLiteralToken)
                return IntLiteralNode(token.value)
            case NonKwSymbolToken() as token:
                return self.var_atom()
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

    # TODO attributes
    # var_atom: SYMBOL ((LPAREN arg_list RPAREN))*
    def var_atom(self):
        name = self._expect(NonKwSymbolToken).symbol_string
        left = VariableNode(name)

        possible_tokens = [LParenToken]
        while any(isinstance(self.file.current_token(), type) for type in possible_tokens):
            if isinstance(self.file.current_token(), LParenToken):
                self._expect(LParenToken)
                args = self.arg_list()
                self._expect(RParenToken)
                left = FuncCallNode(left.name, args)
        return left


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
