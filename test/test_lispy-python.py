import ast
from importlib.machinery import SourceFileLoader
from textwrap import dedent
lp = SourceFileLoader("lispy-python", "lispy-python.py").load_module()

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
