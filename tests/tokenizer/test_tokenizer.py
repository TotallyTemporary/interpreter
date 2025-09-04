import pytest
# from tokenizer.utils import assert_token_types

from tokenizer.tokenizer import Tokenizer
from tokenizer.tokens import *


def test_tokenizer_1():
    tokenizer = Tokenizer("abc")
    tokens = tokenizer.tokens()
    # assert_token_types(tokens, [SymbolToken, EofToken])