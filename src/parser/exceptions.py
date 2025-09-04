
class ParserException(Exception):
    def __init__(self, token, reason):
        super().__init__(f"Parser exception on token {token} at line {token.linenum} at position {token.linepos}: {reason}")

class UnexpectedEofException(ParserException):
    def __init__(self, token):
        super().__init__(token, f"Unexpected EOF")

class UnexpectedTokenException(ParserException):
    def __init__(self, token, expected_type):
        super().__init__(token, f"Unexpected token, expected {expected_type}")