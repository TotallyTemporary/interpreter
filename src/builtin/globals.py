import inspect
from typing import get_type_hints
from parser.symbol import Symbol, FuncSymbol, TypeSymbol, ClassSymbol, ScopedSymbolTable, SymbolTable

GLOBAL_SCOPE = ScopedSymbolTable("global")

FUNC_TYPE = TypeSymbol("__function")
CLASS_TYPE = TypeSymbol("__class")
VOID_TYPE = TypeSymbol("void")

INT_TYPE = TypeSymbol("int")
BOOL_TYPE = TypeSymbol("bool")

for symbol in [FUNC_TYPE, CLASS_TYPE, VOID_TYPE, INT_TYPE, BOOL_TYPE]:
    GLOBAL_SCOPE.define(symbol)

# collect everything into this global list
NATIVE_FUNCTIONS = []

class NativeFunction:
    def __init__(self, name: str, return_type: Symbol, args: list[Symbol], handle):
        self.name = name
        self.return_type = return_type
        self.args = args
        self.handle = handle

        self.symbol = FuncSymbol(self.name, FUNC_TYPE, self.return_type, self.args)

# Decorator to add a function to our native functions list
def native(_func):
    # Handle name
    func = inspect.unwrap(_func) # remove decoration
    name = func.__name__
    name = name.removeprefix("_")

    # Handle argument types
    HINT_TO_SYMBOL = {
        int: INT_TYPE,
        bool: BOOL_TYPE,
        type(None): VOID_TYPE
    }
    hints = get_type_hints(func, include_extras=True)
    arg_hints = [value for key, value in hints.items() if key != 'return']
    arg_types = [HINT_TO_SYMBOL[hint] for hint in arg_hints]

    # Handle return type
    return_hint = next(value for key, value in hints.items() if key == 'return')
    return_type = HINT_TO_SYMBOL[return_hint]

    native_func = NativeFunction(name, return_type, arg_types, func)
    NATIVE_FUNCTIONS.append(native_func)

    return _func

class Globals():
    @staticmethod
    def global_vars() -> list[Symbol]:
        return [func.symbol for func in NATIVE_FUNCTIONS]

class NativeFuncs():
    @staticmethod
    def is_native_func(symbol: Symbol) -> bool:
        for s in Globals.global_vars():
            if s == symbol:
                return True
        return False 
        
    @staticmethod
    def construct_native_funcs():
        return {func.symbol: func.handle for func in NATIVE_FUNCTIONS}
    
@native
def _print(x: int) -> None:
    print(x)