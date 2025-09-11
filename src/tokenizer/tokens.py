

class Token():
    def __init__(self, file):
        self.linenum = file.linenum
        self.linepos = file.linepos

class SimpleToken(Token):
    def __init__(self, file, chars):
        super().__init__(file)
        self.chars = chars

class PlusToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "+")

class MinusToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "-")

class StarToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "*")

class SlashToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "/")

class LParenToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "(")

class RParenToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, ")")

class CommaToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, ",")

class AssignToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "=")

class EqualsToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "==")

class NotEqualsToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "!=")

class LteToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "<=")

class LtToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, "<")

class GteToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, ">=")

class GtToken(SimpleToken):
    def __init__(self, file):
        super().__init__(file, ">")

# -- valued
class ValueToken(Token):
    def __init__(self, file, value):
        super().__init__(file)
        self.value = value

class IntLiteralToken(ValueToken):
    def __init__(self, file, value: int):
        super().__init__(file, value)
# --

# -- symbols
class SymbolToken(Token):
    def __init__(self, file, chars: str):
        super().__init__(file)
        self.chars = chars

class NonKwSymbolToken(SymbolToken):
    def __init__(self, file, chars: str):
        super().__init__(file, chars)

class KeywordToken(SymbolToken):
    def __init__(self, file, chars: str):
        super().__init__(file, chars)

class IfToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "if")

class WhileToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "while")

class IsToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "is")

class DoToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "do")

class OrToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "or")

class ThenToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "then")

class ReturnToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "return")

class AndToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "and")

class NotToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "not")

class TrueToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "true")

class FalseToken(KeywordToken):
    def __init__(self, file):
        super().__init__(file, "false")

# --

# -- indent

class IndentToken(Token):
    def __init__(self, file):
        super().__init__(file)

class DeIndentToken(Token):
    def __init__(self, file):
        super().__init__(file)

# --


# -- control

class EofToken(Token):
    def __init__(self, file):
        super().__init__(file)

# --