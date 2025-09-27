from __future__ import annotations
import operator
from typing import Any, Optional

class StackValue:
    def __init__(self, key: Optional[Any] = None, expr: Optional[str] = None):
        self._key = key
        self._expr = expr

    @staticmethod
    def unknown(expr: Optional[str] = None) -> "StackValue":
        return StackValue(None, expr)

    @staticmethod
    def const(n: Any, expr: Optional[str] = None) -> "StackValue":
        return StackValue(n, expr or repr(n))

    @staticmethod
    def add(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a+b, a, b, opname="(+)")
    
    @staticmethod
    def sub(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a-b, a, b, opname="(-)")
    
    @staticmethod
    def mul(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a*b, a, b, opname="(*)")
    
    @staticmethod
    def div(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a/b, a, b, opname="(/)")
    
    @staticmethod
    def eq(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a==b, a, b, opname="(==)")
    
    @staticmethod
    def neq(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a!=b, a, b, opname="(!=)")
    
    @staticmethod
    def lt(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a<b, a, b, opname="(<)")

    @staticmethod
    def lte(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a<=b, a, b, opname="(<=)")

    @staticmethod
    def gt(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a>b, a, b, opname="(>)")

    @staticmethod
    def gte(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: a>=b, a, b, opname="(>=)")

    @staticmethod
    def oor(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: bool(a) or bool(b), a, b, opname="(or)")
    
    @staticmethod
    def aand(a: "StackValue", b: "StackValue") -> "StackValue":
        return StackValue._const_fold(lambda a, b: bool(a) and bool(b), a, b, opname="(and)")

    @staticmethod
    def _const_fold(op, a: "StackValue", b: "StackValue", opname: str) -> "StackValue":
        expr = f"{a._expr or '?'} {opname} {b._expr or '?'}"
        if a.is_known and b.is_known:
            try:
                res = op(a._key, b._key)
                if isinstance(res, bool):
                    res = 1 if res else 0
                return StackValue.const(res, expr)
            except Exception:
                pass
        return StackValue.unknown(expr)

    @property
    def is_known(self) -> bool:
        return self._key is not None

    @property
    def key(self) -> Optional[Any]:
        return self._key

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, StackValue):
            return NotImplemented
        if self.is_known and other.is_known:
            return self._key == other._key
        return self is other

    def __hash__(self) -> int:
        return hash(self._key) if self.is_known else id(self)

    def __repr__(self) -> str:
        if self.is_known:
            return f"Value(known={self._key})"
        return f"Value(unknown@{hex(id(self))}, expr={self._expr!r})"
