class TokenizerException(Exception):
    def __init__(self, file, reason):
        super().__init__(f"A tokenizer error happened at line {file.linenum} at position {file.linepos}: {reason}")

class InvalidIntegerLiteralException(TokenizerException):
    def __init__(self, file, int_str):
        super().__init__(file, f"Invalid integer literal: '{int_str}'")

class InvalidSymbolException(TokenizerException):
    def __init__(self, file, symbol_str):
        super().__init__(file, f"Invalid symbol: '{symbol_str}'")

class UnexpectedCharacterException(TokenizerException):
    def __init__(self, file, char):
        super().__init__(file, f"Unexpected character: '{char}'")

class InvalidDeindentException(TokenizerException):
    def __init__(self, file, indent_list):
        super().__init__(file, f"Bad deindent. {indent_list=}")