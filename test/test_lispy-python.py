import io
import os
import ast
import sys
from importlib.machinery import SourceFileLoader
from contextlib import redirect_stdout
from textwrap import dedent

lp = SourceFileLoader("lispy-python", os.path.dirname(__file__) + "/../lispy-python.py").load_module()

def with_output_to_string(code):
    with io.StringIO() as buf, redirect_stdout(buf):
        exec(code)
        return buf.getvalue().strip()

def lp_eval(code, env={}):
    return with_output_to_string(f"__res__=lp.eval_code('''{code}''', {env})")

def test_exec():
    # Need to run this with globals().
    # Otherwise, x will not be defined later
    exec("x=1", globals())
    assert x == 1

def test_tr_returns_1():
    code_1 = dedent("""
        x=1
        y=2
        return x + y""")
    parsed = ast.parse(code_1).body
    assert len(parsed) == 3
    translated = lp.tr_returns(parsed)
    assert len(translated) == 3
    assert parsed[0] == translated[0]
    assert parsed[1] == translated[1]
    assert ast.unparse(translated[2]) == "return locals() | {'__return__': x + y}"
    code = ast.unparse(lp.wrap_return(translated))
    exec(code, globals())
    assert x == 1
    assert y == 2
    assert __return__ == 3

def test_tr_returns_2():
    code_2 = dedent("""
        if os.environ.get("FOO"):
            return 0
        x = 1
        y = 2
        return x + y""")
    parsed = ast.parse(code_2).body
    assert len(parsed) == 4
    translated = lp.tr_returns(parsed)
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

def test_translate_def():
    code = dedent("""
    def add(x, y):
        return x + y
    """)
    tr = lp.translate(code)
    assert len(tr) == 1
    assert isinstance(tr[0], ast.FunctionDef)
    r = lp.eval_code(code)
    assert r["res"] == "'unset'"
    assert "<function add at " in r["binds"]["add"]
    assert r["out"] == ""
    assert r["err"] is None

def test_file_fname():
    r = lp.eval_code("__file__", {"fname": "shrubbery"})
    assert r["res"] == "'shrubbery'"
    assert r["binds"] == {}
    assert r["out"] == ""
    assert r["err"] is None

def test_translate_return_1():
    code = dedent("""
    x = 1
    y = 2
    return x + y
    """)
    r = lp.eval_code(code)
    assert r["res"] == "3"
    assert r["binds"] == {"x": "1", "y": "2"}
    assert r["out"] == ""
    assert r["err"] is None

def test_translate_return_2():
    code = dedent("""
    if False:
        x = 1
        y = 2
        return x + y
    else:
        return 5
    """)
    r = lp.eval_code(code)
    assert r["res"] == "5"
    assert r["binds"] == {}
    assert r["out"] == ""
    assert r["err"] is None

def test_translate_return_3():
    lp.eval_code("l = []")
    code = dedent("""
    if not isinstance(l, list):
        return False
    """)
    print(ast.unparse(lp.translate(code)))
    r = lp.eval_code(code)
    print(f"{r=}")
    assert r["res"] == "None"
    lp.eval_code("l = 1")
    assert lp.eval_code(code)["res"] == "False"

def test_eval_print():
    r = lp.eval_code("print('hello')")
    assert r["res"] == "None"
    assert r["binds"] == {}
    assert r["out"] == "hello"

def test_eval_bind_var():
    r = lp.eval_code("x = 2 + 2")
    assert r["res"] == "'unset'"
    assert r["binds"]["x"] == "4"
    assert r["out"] == ""
    assert lp.eval_code("x")["res"] == "4"

def test_eval_bind_vars_1():
    code = "(v1, v2, v3) = (1, 2, 3)"
    r = lp.eval_code(code)
    assert r["res"] == "'unset'"
    binds = r["binds"]
    assert binds["v1"] == "1"
    assert binds["v2"] == "2"
    assert binds["v3"] == "3"

def test_eval_bind_vars_2():
    code = dedent("""
    if True:
        x = 42
    else:
        x = 10
    """)
    r = lp.eval_code(code)
    assert r["res"] == "'unset'"
    assert  r["binds"] == {'x': '42'}


def test_eval_in_1():
    env = {"frame": sys._getframe(), "use-in-expr": True}
    r1 = lp.eval_code("xs = [1, 2, 3]", env)
    code = "x in xs"
    r = lp.eval_code(code, env)
    assert r["res"] == "'select'"
    assert r["out"] == "(1\n2\n3\n)"

def test_eval_in_2():
    env = {"frame": sys._getframe(), "use-in-expr": True}
    code = "x in [1, 2, 3]"
    r = lp.eval_code(code, env)
    assert r["res"] == "'select'"
    assert r["out"] == "(1\n2\n3\n)"
    assert lp.select_item("x in [1, 2, 3]", 2, env["frame"]) == 3

def test_eval_in_3():
    env = {"frame": sys._getframe(), "use-in-expr": True}
    lp.eval_code("di = {'foo': 'bar', 'yes': 'no'}", env)
    code = "(k, v) in di.items()"
    r = lp.eval_code(code, env)
    assert r["res"] == "'select'"
    assert lp.select_item(code, 0) == ('foo', 'bar')
    assert lp.eval_code("k")["res"] == "'foo'"
    assert lp.eval_code("v")["res"] == "'bar'"
    assert lp.select_item(code, 1) == ('yes', 'no')
    assert lp.eval_code("k")["res"] == "'yes'"
    assert lp.eval_code("v")["res"] == "'no'"

def test_eval_in_off():
    env = {"frame": sys._getframe(), "use-in-expr": False}
    lp.eval_code('xs = ["1", "2", "3"]', env)
    lp.eval_code('x = "3"', env)
    r = lp.eval_code("x in xs", env)
    assert r["res"] == "True"
    assert r["binds"] == {}

def test_eval_in_pytest_1():
    env = {"frame": sys._getframe(), "use-in-expr": True}
    code = dedent("""
    @pytest.mark.parametrize("x", [3, 4, 5])
    def square(x):
        return x*x
    """)
    r = lp.eval_code(code, env)
    assert r["res"] == "'select'"
    assert r["out"] == '(3\n4\n5\n)'
    assert lp.select_item(code, 0) == 3
    assert lp.select_item(code, 1) == 4
