import logging
from parser.nodes import *

log = logging.getLogger(__name__)

class TypeCheckerException(Exception):
    def __init__(self, reason):
        super().__init__(f"Type checker error: {reason}")

class Symbol():
    def __init__(self, name: str, type: "Symbol" = None):
        self.name = name
        self.type = type

    def __repr__(self):
        return f"Symbol({self.name},{self.type})"

class ClassSymbol(Symbol):
    def __init__(self, name: str, type: "Symbol" = None):
        super().__init__(name, type)
        self.inside: ScopedSymbolTable | None = None # filled by type checker
        self.fields: list[Symbol] | None = None

class ClassFieldSymbol(Symbol):
    def __init__(self, name: str, type: "Symbol" = None, class_type: "ClassSymbol" = None):
        self.name = name
        self.type = type
        self.class_type = class_type

    def __repr__(self):
        return f"Field({self.name},{self.type},{self.class_type})"

class FuncSymbol(Symbol):
    def __init__(self, name: str, type: "Symbol" = None, return_type: "Symbol" = None, arg_symbols: list["Symbol"] = None):
        super().__init__(name, type)
        self.return_type = return_type
        self.arg_symbols = arg_symbols

class TypeSymbol(Symbol):
    def __init__(self, name: str):
        super().__init__(name, None)

class SymbolTable():
    def __init__(self, name: str):
        self.name = name
        self.symbols = {}

    def define(self, symbol: Symbol):
        log.debug(f"Defining {symbol}")

        if symbol.name in self.symbols and self.symbols[symbol.name] is not symbol:
            raise TypeCheckerException(f"Double declaration of symbol {symbol.name}")
        self.symbols[symbol.name] = symbol

    def lookup(self, symbol_name: str, error_on_not_found=True) -> Symbol | None:
        log.debug(f"Looking up {symbol_name}")

        value = self.symbols.get(symbol_name, None)

        if value is None and error_on_not_found:
            raise TypeCheckerException(f"Symbol not found {symbol_name}")
        return value
    
class ScopedSymbolTable(SymbolTable):
    def __init__(self, name: str, parent: "ScopedSymbolTable" = None):
        super().__init__(name)
        self.parent = parent
        self.children = []
        if self.parent is not None:
            self.parent.add_child(self)

    def add_child(self, child: "ScopedSymbolTable"):
        self.children.append(child)

    def define(self, symbol: Symbol):
        super().define(symbol)

    def lookup(self, symbol_name: str, error_on_not_found=True) -> Symbol:
        value = super().lookup(symbol_name, error_on_not_found=False)
        if not value and self.parent is not None:
            value = self.parent.lookup(symbol_name)
        
        if value is None and error_on_not_found:
            raise TypeCheckerException(f"Symbol not found: {symbol_name}")
        return value