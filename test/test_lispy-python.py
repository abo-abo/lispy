import os
import ast
from importlib.machinery import SourceFileLoader
from textwrap import dedent
lp = SourceFileLoader("lispy-python", "lispy-python.py").load_module()

def test_exec():
    # Need to run this with globals().
    # Otherwise, x will not be defined later
    exec("x=1", globals())
    assert x == 1

def test_translate_returns_1():
    code_1 = dedent("""
        x=1
        y=2
        return x + y""")
    parsed = ast.parse(code_1).body
    assert len(parsed) == 3
    translated = lp.translate_returns(parsed)
    assert len(translated) == 3
    assert parsed[0] == translated[0]
    assert parsed[1] == translated[1]
    assert ast.unparse(translated[2]) == "return locals() | {'__return__': x + y}"
    code = ast.unparse(lp.wrap_return(translated))
    exec(code, globals())
    assert x == 1
    assert y == 2
    assert __return__ == 3

def test_translate_returns_2():
    code_2 = dedent("""
        if os.environ.get("FOO"):
            return 0
        x = 1
        y = 2
        return x + y""")
    parsed = ast.parse(code_2).body
    assert len(parsed) == 4
    translated = lp.translate_returns(parsed)
    assert len(translated) == 4
    assert ast.unparse(translated[3]) == "return locals() | {'__return__': x + y}"
    assert ast.unparse(translated[0].body) == "return locals() | {'__return__': 0}"
    code = ast.unparse(lp.wrap_return(translated))
    exec(code, globals())
    assert x == 1
    assert y == 2
    assert __return__ == 3
    os.environ["FOO"] = "BAR"
    exec(code, globals())
    assert __return__ == 0

def test_translate_assign_1():
    code = "x = 3"
    parsed = ast.parse(code).body
    assert ast.unparse(lp.translate_assign(parsed)[-1]) == "print(x)"

def test_translate_assign_2():
    code = "x"
    parsed = ast.parse(code).body
    parsed[-1].value
    assert ast.unparse(lp.translate_assign(parsed)) == "print(\nx)"

def test_translate_def():
    code = dedent("""
    def add(x, y):
        return x + y
    """)
    tr = lp.translate(code)
    assert len(tr) == 2
    assert isinstance(tr[0], ast.FunctionDef)
    assert ast.unparse(tr[1]) == "print('(ok)')"
