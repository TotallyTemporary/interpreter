import string
from tokenizer.tokens import *
from tokenizer.exceptions import *
from typing import Optional

EOF = None
SYMBOL_ALLOWED_CHARS = string.ascii_letters + string.digits + "_"

class TokenizerFile:
    def __init__(self, input: str):
        self.input = input
        self.index = 0
        self.linenum = 1
        self.linepos = 0

    def current_char(self) -> str:
        if self.index >= len(self.input):
            return None
        return self.input[self.index]

    def next_char(self) -> str:
        if self.index + 1 >= len(self.input):
            return None
        return self.input[self.index + 1]

    def advance(self) -> str:
        current_char = self.current_char()
        self.index += 1

        if current_char == "\n":
            self.linenum += 1
            self.linepos = 0
        else:
            self.linepos += 1


class Tokenizer:
    def __init__(self, input: str):
        self.file = TokenizerFile(input)
        self.indents = []

    def tokens(self) -> list[Token]:
        tokens = []
        while True:
            token = self.next_token()
            tokens.append(token)
            if type(token) == EofToken:
                return tokens

    def next_token(self) -> Token:
        if deindent := self._handle_deindents():
            return deindent
        if indent := self._handle_newlines_and_indents():
            return indent
        self._handle_whitespace()
        if deindent := self._handle_eof_deindents():
            return deindent

        if eof := self._handle_eof():
            return eof
        
        # By this point:
        # - Not EOF
        # - Not Whitespace

        if simple_token := self._handle_simple_tokens():
            return simple_token
        
        if symbol := self._handle_symbols():
            return symbol
        
        if int_lit := self._handle_int_literals():
            return int_lit

        raise UnexpectedCharacterException(self.file, self.file.current_char())

    def _handle_symbols(self) -> Optional[Token]:
        # a symbol can't begin with an integer, but can contain it later
        if self.file.current_char().isnumeric():
            return None

        symbol_str = ""
        while self.file.current_char() is not EOF and self.file.current_char() in SYMBOL_ALLOWED_CHARS:
            symbol_str += self.file.current_char()
            self.file.advance()
        
        if symbol_str == "":
            return None
        
        # check for keywords
        keyword_tokens = [Subclass(self.file) for Subclass in KeywordToken.__subclasses__()]
        for keyword_token in keyword_tokens:
            if keyword_token.symbol_string == symbol_str:
                return keyword_token
    
        return NonKwSymbolToken(self.file, symbol_str)

    def _handle_int_literals(self) -> Optional[Token]:
        int_str = ""
        while self.file.current_char() is not EOF and self.file.current_char().isnumeric():
            int_str += self.file.current_char()
            self.file.advance()

        if len(int_str) != 0:
            value: int
            try:
                value = int(int_str)
            except ValueError as e:
                raise InvalidIntegerLiteralException(self.file, int_str) from e
            return IntLiteralToken(self.file, value)

    def _handle_eof(self) -> Optional[Token]:
        if self.file.current_char() is EOF:
            return EofToken(self.file)
        return None

    def _handle_eof_deindents(self) -> Optional[Token]:
        """Create deindent tokens when we reach end-of-file."""
        if self.file.current_char() == EOF and len(self.indents) != 0:
            del self.indents[-1]
            return DeIndentToken(self.file)

    def _handle_deindents(self) -> Optional[Token]:
        """A negative indent is in the indentation list. Keep collapsing out indents until we're done.
        Example:
            self.indents = [2, 2, -4] should emit two deindents, we're breaking out of two scopes.
        """
        latest = self.indents[-1] if len(self.indents) > 0 else 0

        if latest < 0:
            # check if the removal goes nicely
            rev = list(reversed(self.indents))
            for subset_len in range(1, len(self.indents) + 1):
                if sum(rev[:subset_len]) == 0:
                    break
            else:
                raise InvalidDeindentException(self.file, self.indents)
            
            # deindent by one - remove the enclosing scope and add it to our deindent
            enclosing_scope = self.indents[-2]
            del self.indents[-2]
            self.indents[-1] += enclosing_scope

            # have we deindented enough? have we broken out of all scopes necessary?
            if self.indents[-1] == 0:
                del self.indents[-1]

            return DeIndentToken(self.file)


    def _handle_newlines_and_indents(self) -> Optional[Token | list[Token]]:
        # start of a new line (from ending previous line or from starting the file)
        while self.file.current_char() == "\n" or self.file.index == 0:
            if self.file.current_char() == "\n":
                self.file.advance()

            # count up the indent
            indent = 0
            while (
                self.file.current_char() is not EOF
                and self.file.current_char().isspace()
                and self.file.current_char() != "\n" # handled separately
            ):
                indent += 1
                self.file.advance()

            # fully whitespace line
            if self.file.current_char() == "\n" or self.file.current_char() == EOF:
                continue
            
            prev_indent = sum(self.indents)
            diff = indent - prev_indent
            if diff > 0:
                self.indents.append(diff)
                return IndentToken(self.file)
            elif diff < 0:
                # we might need to emit multiple tokens, store this data and handle it every subsequent call
                self.indents.append(diff)
                return self._handle_deindents()

            return None

    def _handle_whitespace(self) -> None:
        while (self.file.current_char() is not EOF and self.file.current_char().isspace()):
            self.file.advance()

    def _handle_simple_tokens(self) -> Optional[Token]:
        simple_tokens = [
            token_class(self.file) for token_class in SimpleToken.__subclasses__()
        ]  # initialize
        two_char_tokens = [token for token in simple_tokens if len(token.chars) == 2]
        one_char_tokens = [token for token in simple_tokens if len(token.chars) == 1]

        if self.file.next_char() != EOF:
            for token in two_char_tokens:
                if token.chars == self.file.current_char() + self.file.next_char():
                    self.file.advance()
                    self.file.advance()
                    return token

        if self.file.current_char() != EOF:
            for token in one_char_tokens:
                if token.chars == self.file.current_char():
                    self.file.advance()
                    return token

        return None

    def _expect(self, expected_char):
        current_char = self.file.current_char()
        if current_char != expected_char:
            raise UnexpectedCharacterException(self.file, current_char)
        self.file.advance()
