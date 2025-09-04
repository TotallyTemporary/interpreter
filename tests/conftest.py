def assert_token_types(tokens, expected_types):
    for token, type in zip(tokens, expected_types, strict=True):
        assert isinstance(token, type)