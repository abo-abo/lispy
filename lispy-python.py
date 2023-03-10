# lispy-python.py --- lispy support for Python.

# Copyright (C) 2016-2019 Oleh Krehel

# This file is not part of GNU Emacs

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# For a full copy of the GNU General Public License
# see <http://www.gnu.org/licenses/>.

#* Imports
import ast
import collections
import importlib
import inspect
import io
import json
import os
import pprint as pp
import re
import shlex
import subprocess
import sys
from ast import AST
from contextlib import redirect_stdout
from typing import List, Dict, Any, Union, Tuple, Optional, TypedDict, Callable, cast
from types import TracebackType, MethodType, FunctionType, ModuleType, FrameType

def sh(cmd: str) -> str:
    r = subprocess.run(
        shlex.split(cmd),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding="utf-8",
        check=True)
    return r.stdout.strip()

try:
    import reprlib
    repr1 = reprlib.Repr()
    repr1.maxlist = 10
    repr1.maxstring = 200
except:
    pass
try:
    import jedi
except:
    try:
        pyenv_version = sh("pyenv global")
        pyversion = ".".join(pyenv_version.split(".")[:-1])
        site_packages = os.path.expanduser(f"~/.pyenv/versions/{pyenv_version}/lib/python{pyversion}/site-packages/")
        sys.path.append(site_packages)
        import jedi
    except:
        print("Failed to load jedi. Some features won't work")

#* Classes
class Stack:
    line_numbers: Dict[Tuple[str, str], int] = {}

    def __init__(self, tb: Optional[TracebackType]):
        self.stack = []
        self.stack_idx = 0
        while tb:
            name = tb.tb_frame.f_code.co_name
            fname = tb.tb_frame.f_code.co_filename
            if (fname, name) in Stack.line_numbers:
                lineno = Stack.line_numbers[(fname, name)]
            else:
                lineno = tb.tb_frame.f_lineno
            self.stack.append((fname, lineno, tb.tb_frame))
            tb = tb.tb_next
        self.stack_top = len(self.stack) - 1
        if self.stack_top >= 0:
            self.set_frame(self.stack_top)

    def frame_string(self, i: int) -> str:
        (fname, line, f) = self.stack[i]
        res = "  File \"%s\", line %d, Frame [%d/%d] (%s):" % (
            f.f_code.co_filename, line, i, self.stack_top, f.f_code.co_name)
        return res

    def __repr__(self) -> str:
        frames = []
        for i in range(self.stack_top + 1):
            s = self.frame_string(i)
            if i == self.stack_idx:
                s += "*"
            frames.append(s)
        return "\n".join(frames)

    def set_frame(self, i: int) -> None:
        if i >= 0:
            f = self.stack[i][2]
            self.stack_idx = i
            tf = top_level()
            tf.f_globals["lnames"] = f.f_locals.keys()
            for (k, v) in f.f_locals.items():
                tf.f_globals[k] = v
            for (k, v) in f.f_globals.items():
                tf.f_globals[k] = v

            print(self.frame_string(self.stack_idx))

    def up(self, delta: int = 1) -> None:
        if self.stack_idx <= 0:
            if self.stack:
                print(self.frame_string(self.stack_idx))
        else:
            self.stack_idx = max(self.stack_idx - delta, 0)
            self.set_frame(self.stack_idx)

    def down(self, delta: int = 1) -> None:
        if self.stack_idx >= self.stack_top:
            if self.stack:
                print(self.frame_string(self.stack_idx))
        else:
            self.stack_idx = min(self.stack_idx + delta, self.stack_top)
            self.set_frame(self.stack_idx)

class Autocall:
    def __init__(self, f: Callable):
        self.f = f

    def __call__(self, n: Any) -> None:
        self.f(n)

    def __repr__(self) -> str:
        try:
            self.f()
        except:
            pass
        return ""

#* Functions
def get_import_name(fname: str) -> str:
    for p in sys.path:
        if p == "":
            continue
        if fname.startswith(p):
            return fname[len(p) + 1:].partition(".")[0].replace("/", ".")
    return os.path.splitext(os.path.basename(fname))[0]

def chfile(f: str) -> None:
    tf = top_level()
    tf.f_globals["__file__"] = f
    name = get_import_name(f)
    tf.f_globals["__name__"] = name
    d = os.path.dirname(f)
    try:
        os.chdir(d)
        if "sys" not in tf.f_globals:
            tf.f_globals["sys"] = importlib.import_module("sys")
        if name not in tf.f_globals["sys"].modules:
            try:
                mod = importlib.import_module(name)
                tf.f_globals["sys"].modules[name] = mod
            except:
                pass
    except:
        raise

def arglist(sym: Callable) -> List[str]:
    def format_arg(arg_pair: Tuple[str, Optional[str]]) -> str:
        name, default_value = arg_pair
        if default_value:
            return name + " = " + default_value
        else:
            return name
    arg_info = inspect.getfullargspec(sym)
    if "self" in arg_info.args:
        arg_info.args.remove("self")
    if arg_info.defaults:
        defaults: List[Optional[str]] = [None] * (len(arg_info.args) - len(arg_info.defaults))
        defaults += [repr(x) for x in arg_info.defaults]
        args = [format_arg(x) for x in zip(arg_info.args, defaults)]
    else:
        args = arg_info.args
        if arg_info.varargs:
            args += arg_info.varargs
    keywords = arg_info.kwonlydefaults
    if keywords:
        for k, v in keywords.items():
            args.append(f"{k} = {v}")
    return args

def print_elisp(obj: Any, end: str = "\n") -> None:
    if hasattr(obj, "_asdict") and obj._asdict is not None:
        if hasattr(type(obj), "__repr__"):
            print('"' + str(obj).replace('"', '') + '"')
            return
        # namedtuple
        try:
            print_elisp(obj._asdict(), end)
        except:
            print('"' + str(obj) + '"')
    elif hasattr(obj, "__array__"):
        # something that converts to a numpy array
        print_elisp(list(obj.__array__()))
    elif isinstance(obj, enumerate):
        print("(")
        for (i, v) in list(obj):
            print("(", end="")
            print_elisp(v, end="")
            print(")")
        print(")")
    elif isinstance(obj, set):
        print("(")
        for v in obj:
            print_elisp(v, end=" ")
        print(")")
    elif isinstance(obj, dict):
        print("(")
        for (k, v) in obj.items():
            print("  :" + k, end=" ")
            print_elisp(v, end="\n")
        print(")")
    elif isinstance(obj, collections.abc.ItemsView):
        print("(")
        for (k, v) in obj:
            print_elisp((k, v), end="\n")
        print(")")
    elif isinstance(obj, collections.abc.KeysView):
        print_elisp(list(obj))
    elif isinstance(obj, int):
        print(obj)
    elif isinstance(obj, list) or isinstance(obj, tuple):
        print("(", end="")
        for x in obj:
            print_elisp(x)
        print(")")
    else:
        if obj is not None:
            if type(obj) is str:
                # quote strings?
                # print("\"'" + re.sub("\"", "\\\"", obj) + "'\"", end=" ")
                print('"' + re.sub("\"", "\\\"", obj) + '"', end=" ")
            elif hasattr(obj, "to_dict"):
                print_elisp(obj.to_dict())
            else:
                print('"' + repr(obj) + '"', end=" ")
        else:
            print('nil', end=end)

def argspec(sym):
    arg_info = inspect.getfullargspec(sym)
    if arg_info:
        di = arg_info._asdict()
        fn = sym.__init__ if type(sym) is type else sym
        try:
            filename = fn.__code__.co_filename
            di["filename"] = filename
            if hasattr(sym, "__self__"):
                # bound method
                qname = sym.__self__.__class__.__name__ + "." + sym.__name__
            else:
                qname = sym.__qualname__
            tu = (filename, qname)
            if tu in Stack.line_numbers:
                di["line"] = Stack.line_numbers[tu]
            else:
                di["line"] = fn.__code__.co_firstlineno
        except AttributeError:
            m = sys.modules[sym.__module__]
            filename = m.__file__
            nodes = ast.parse(open(filename).read()).body
            for node in nodes:
                if (type(node) in [ast.ClassDef, ast.FunctionDef] and
                    node.name == sym.__name__):
                    di["filename"] = filename
                    di["line"] = node.lineno

        print_elisp(di)
    else:
        print("nil")

def arglist_jedi(line, column, filename):
    script = jedi.Script(path=filename)
    defs = script.get_signatures(line, column)
    if defs:
        return [x.name for x in defs[0].params]
    else:
        return []

def jedi_completions(line):
    script = jedi.Script(code=line)
    return [x.name for x in script.complete()]

def jedi_file_completions(fname, line, column):
    script = jedi.Script(path=fname)
    return [x.name for x in script.complete(line, column)]

def is_assignment(code):
    ops = ast.parse(code).body
    return len(ops) == 1 and type(ops[0]) is ast.Assign

def top_level():
    """Return the topmost frame."""
    f = sys._getframe()
    while f.f_back:
        f = f.f_back
        if f.f_code.co_filename == "<console>":
            return f
    return f

def list_step(varname, lst):
    f_globals = top_level().f_globals
    try:
        val = f_globals[varname]
        i = (lst.index(val) + 1) % len(lst)
    except:
        i = 0
    val = lst[i]
    print("[{}/{}]".format(i + 1, len(lst)))
    f_globals[varname] = val
    return val

def argv(cmd: str) -> None:
    sys.argv = shlex.split(cmd)

def find_global_vars(class_name):
    """Find global variables of type CLASS_NAME."""
    return [(k, v) for (k, v) in top_level().f_globals.items() if v.__class__.__name__ == class_name]

def rebind(method, fname=None, line=None):
    """Rebind METHOD named like Class.function in all top level instances of Class.

    Modifying a method is two-step:
    1. eval the method as if it's a free top-level function,
    2. modify all instances of the class with an adapter to this top-level function.
    """
    qname = method.__qualname__
    (cls_name, fun_name) = qname.split(".")
    for (n, v) in find_global_vars(cls_name):
        print("rebind:", n)
        top_level().f_globals[n].__dict__[fun_name] = MethodType(top_level().f_globals[fun_name], v)
    if fname and line:
        Stack.line_numbers[(fname, qname)] = line

def pm() -> None:
    """Post mortem: recover the locals and globals from the last traceback."""
    if hasattr(sys, 'last_traceback'):
        stack = Stack(sys.last_traceback)
    else:
        stack = Stack(sys.exc_info()[2])
    tl = top_level()
    tl.f_globals["up"] = Autocall(stack.up)
    tl.f_globals["dn"] = Autocall(stack.down)
    globals()["stack"] = stack

def pprint(x: Any) -> None:
    r1 = repr(x)
    if len(r1) > 1000 and repr1:
        print(repr1.repr(x))
    else:
        if type(x) == collections.OrderedDict:
            print("{" + ",\n ".join([str(k) + ": " + str(v) for (k, v) in x.items()]) + "}")
        else:
            pp.PrettyPrinter(width=200).pprint(x)

def to_str(x: Any) -> str:
    with io.StringIO() as buf, redirect_stdout(buf):
        pprint(x)
        return buf.getvalue().strip()

def step_in(fn, *args):
    spec = inspect.getfullargspec(fn)
    f_globals = top_level().f_globals
    for (arg_name, arg_val) in zip(spec.args, args):
        f_globals[arg_name] = arg_val

def step_into_module_maybe(module):
    if isinstance(module, FunctionType):
        try:
            module = sys.modules[module.__module__]
        except:
            pass
    elif getattr(module, "__module__", None):
        if module.__module__ == "__repl__":
            return
        module = sys.modules[module.__module__]
    if inspect.ismodule(module):
        tf = top_level()
        for (k, v) in module.__dict__.items():
            if not re.match("^__", k):
                print(k)
                tf.f_globals[k] = v

def slurp(fname: str) -> str:
    """Return `fname' contents as text."""
    with open(fname, "r", encoding="utf-8") as fh:
        return fh.read()

def definitions(path):
    (_, ext) = os.path.splitext(path)
    if ext == ".yml":
        return yaml_definitions(path)
    script = jedi.Script(slurp(path), path=path)
    res = []
    for x in script.get_names():
        if (x.get_definition_start_position()[0] == x.get_definition_end_position()[0]
            and "import" in x.get_line_code()):
            continue
        if x.type == "function":
            try:
                desc = x.description + "(" + ", ".join(p.name for p in x.params) + ")"
            except:
                desc = x.description
            res.append([desc, x.line])
        elif x.type == "module":
            res.append(["import " + x.name, x.line])
        elif x.type == "class":
            res.append([x.description, x.line])
            try:
                members = x.defined_names()
            except:
                members = []
            for m in members:
                res.append([x.name + "." + m.name, m.line])
        else:
            res.append([x.description, x.line])
    return res


def yaml_definitions(path):
    res = []
    ls = slurp(path).strip().splitlines()
    prev = ""
    symbol = "(\\w|[-_])+"
    for (i, line) in enumerate(ls, 1):
        if m := re.match(f"^({symbol})", line):
            res.append([m.group(1), i])
            prev = m.group(1) + "."
        elif m := re.match(f"^  ({symbol})", line):
            res.append([prev + m.group(1), i])
    return res


def get_completions_readline(text):
    completions = []
    completer = None
    try:
        import readline
        # pylint: disable=unused-import
        import rlcompleter
        completer = readline.get_completer()
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = False
        i = 0
        while True:
            completion = completer(text, i)
            if not completion:
                break
            i += 1
            if not re.match("[0-9]__", completion):
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return [re.sub("__t__.", "", c) for c in completions]

def get_completions(text):
    completions = get_completions_readline(text)
    if completions:
        return sorted(completions)
    m = re.match(r"([^.]+)\.(.*)", text)
    if m:
        (obj, part) = m.groups()
        regex = re.compile("^" + part)
        o = top_level().f_globals[obj]
        items = list(o.__dict__.keys()) if hasattr(o, "__dict__") else []
        items += list(type(o).__dict__.keys()) if hasattr(type(o), "__dict__") else []
        for x in set(items):
            if re.match(regex, x):
                if not x.startswith("_") or part.startswith("_"):
                    completions.append(x)
        return sorted(completions)
    else:
        return []

def __PYTHON_EL_native_completion_setup():
    import readline
    try:
        import __builtin__
    except ImportError:
        # Python 3
        import builtins as __builtin__

    builtins = dir(__builtin__)
    is_ipython = ('__IPYTHON__' in builtins or
                  '__IPYTHON__active' in builtins)

    class __PYTHON_EL_Completer:
        '''Completer wrapper that prints candidates to stdout.

        It wraps an existing completer function and changes its behavior so
        that the user input is unchanged and real candidates are printed to
        stdout.

        Returned candidates are '0__dummy_completion__' and
        '1__dummy_completion__' in that order ('0__dummy_completion__' is
        returned repeatedly until all possible candidates are consumed).

        The real candidates are printed to stdout so that they can be
        easily retrieved through comint output redirect trickery.
        '''

        PYTHON_EL_WRAPPED = True

        def __init__(self, completer):
            self.completer = completer
            self.last_completion = None
            self.print_mode = True

        def __call__(self, text, state):
            if state == 0:
                # Set the first dummy completion.
                self.last_completion = None
                completion = '0__dummy_completion__'
            else:
                completion = self.completer(text, state - 1)

            if not completion:
                if self.last_completion != '1__dummy_completion__':
                    # When no more completions are available, returning a
                    # dummy with non-sharing prefix allow ensuring output
                    # while preventing changes to current input.
                    # Coincidentally it's also the end of output.
                    completion = '1__dummy_completion__'
            elif completion.endswith('('):
                # Remove parens on callables as it breaks completion on
                # arguments (e.g. str(Ari<tab>)).
                completion = completion[:-1]
            self.last_completion = completion

            if completion in (
                    '0__dummy_completion__', '1__dummy_completion__'):
                return completion
            elif completion:
                # For every non-dummy completion, return a repeated dummy
                # one and print the real candidate so it can be retrieved
                # by comint output filters.
                if self.print_mode:
                    print(completion)
                    return '0__dummy_completion__'
                else:
                    return completion
            else:
                return completion

    completer = readline.get_completer()

    if not completer:
        # Used as last resort to avoid breaking customizations.
        # pylint: disable=unused-import
        import rlcompleter
        completer = readline.get_completer()

    if completer and not getattr(completer, 'PYTHON_EL_WRAPPED', False):
        # Wrap the existing completer function only once.
        new_completer = __PYTHON_EL_Completer(completer)
        if not is_ipython:
            readline.set_completer(new_completer)
        else:
            # Try both initializations to cope with all IPython versions.
            # This works fine for IPython 3.x but not for earlier:
            readline.set_completer(new_completer)
            # IPython<3 hacks readline such that `readline.set_completer`
            # won't work.  This workaround injects the new completer
            # function into the existing instance directly:
            instance = getattr(completer, 'im_self', completer.__self__)
            instance.rlcomplete = new_completer

    if readline.__doc__ and 'libedit' in readline.__doc__:
        raise Exception('''libedit based readline is known not to work,
  see etc/PROBLEMS under \"In Inferior Python mode, input is echoed\".''')

    readline.parse_and_bind('tab: complete')
    # Require just one tab to send output.
    readline.parse_and_bind('set show-all-if-ambiguous on')


__PYTHON_EL_native_completion_setup()

def setup(init_file=None):
    sys.modules['__repl__'] = sys.modules[__name__]
    tl = top_level()
    tl.f_globals["__name__"] = "__repl__"
    tl.f_globals["pm"] = Autocall(pm)
    if init_file and os.path.exists(init_file):
        try:
            exec(open(init_file).read(), tl.f_globals)
        except:
            pass

def reload():
    import importlib.util
    spec = importlib.util.spec_from_file_location('lispy-python', __file__)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    top_level().f_globals["lp"] = mod
    sys._getframe().f_back.f_globals["lp"] = mod
    sys._getframe().f_back.f_locals["lp"] = mod
    return mod

def reload_module(fname):
    to_reload = []
    for (name, module) in sys.modules.copy().items():
        try:
            if module.__dict__.get("__file__") == fname and name != "__main__":
                to_reload.append((name, module))
        except:
            pass
    for (name, module) in to_reload:
        try:
            importlib.reload(module)
        except:
            pass

def goto_definition(fname: str, line: int, column: int) -> None:
    d = jedi.Script(path=fname).goto(line, column)
    if d:
        print_elisp((str(d[0].module_path), d[0].line, d[0].column))

def goto_link_definition(c: str) -> None:
    module = c.split(".")[0]
    d = jedi.Script(code=f"import {module}\n{c}").goto(2, len(c) - 2, follow_imports=True)
    if d:
        print_elisp((str(d[0].module_path), d[0].line, d[0].column))

def ast_pp(code: str) -> str:
    parsed = ast.parse(code, mode="exec")
    return ast.dump(parsed.body[0], indent=4)

Expr = Union[List[ast.stmt], ast.stmt, Any]

def has_return(p: Expr) -> bool:
    if isinstance(p, list):
        return any(has_return(x) for x in p)
    if isinstance(p, ast.Return):
        return True
    elif isinstance(p, ast.If):
        return has_return(p.body) or has_return(p.orelse)
    else:
        return False

def tr_returns(p: Expr) -> Expr:
    if isinstance(p, list):
        return [tr_returns(x) for x in p]
    if isinstance(p, ast.Return) and p.value:
        return ast.parse("return locals() | {'__return__': " + ast.unparse(p.value) + "}").body[0]
    elif isinstance(p, ast.If):
        return ast.If(
            test=p.test,
            body=tr_returns(p.body),
            orelse=tr_returns(p.orelse))
    else:
        return p

def ast_call(func: Union[str, AST], args: List[Any] = [], keywords: List[Any] = []):
    if isinstance(func, str):
        func = ast.Name(func)
    return ast.Call(func=func, args=args, keywords=keywords)

def wrap_return(parsed: List[ast.stmt]) -> Expr:
    return [
        ast.FunctionDef(
            name="__res__",
            body=[*parsed, *ast.parse("return {'__return__': None}").body],
            decorator_list=[],
            args=[],
            lineno=0,
            col_offset=0),
        ast.Expr(
            ast_call(
                ast.Attribute(value=ast_call("locals"), attr="update"),
                args=[ast_call("__res__")])),
        ast.Expr(value=ast.Name("__return__"))
    ]

def try_in_expr(p: Expr) -> Optional[Tuple[ast.expr, ast.expr]]:
    if not isinstance(p, list):
        return None
    if pytest_mark := try_pytest_mark(p):
        return pytest_mark
    p0 = p[0]
    if not isinstance(p0, ast.Expr):
        return None
    if not isinstance(p0.value, ast.Compare):
        return None
    if not isinstance(p0.value.ops[0], ast.In):
        return None
    return (p0.value.left, p0.value.comparators[0])

def select_item(code: str, idx: int, _f: Optional[FrameType] = None) -> Any:
    _f = _f or sys._getframe().f_back
    parsed = ast.parse(code, mode="exec").body
    in_expr = try_in_expr(parsed)
    assert in_expr
    (left, right) = in_expr
    l = ast.unparse(left)
    r = ast.unparse(right)
    locals_1 = locals()
    locals_2 = locals_1.copy()
    # pylint: disable=exec-used
    exec(f"{l} = list({r})[{idx}]", _f.f_locals | _f.f_globals, locals_2)
    for bind in [k for k in locals_2.keys() if k not in locals_1.keys()]:
        _f.f_globals[bind] = locals_2[bind]
    # pylint: disable=eval-used
    return eval(l, locals_2)

def ast_match(p: Expr, expr: Any) -> bool:
    if isinstance(p, ast.Attribute):
        if expr[0] != ".":
            return False
        return (
            expr[2] == p.attr
            and ast_match(p.value, expr[1]))
    elif isinstance(p, ast.Name):
        return p.id == expr
    elif isinstance(p, str):
        return p == expr
    else:
        raise RuntimeError(f"Can't compare: {p} == {expr}")

def try_pytest_mark(p: Expr) -> Optional[Expr]:
    if not isinstance(p, list):
        return None
    if not len(p) == 1:
        return None
    p0 = p[0]
    if not isinstance(p0, ast.FunctionDef):
        return None
    if not len(p0.decorator_list) == 1:
        return None
    decorator = p0.decorator_list[0]
    if ast_match(decorator.func, (".",  (".", "pytest", "mark"), "parametrize")):
        assert len(decorator.args) == 2
        return [ast.Name(decorator.args[0].value), decorator.args[1]]
    return None

def to_elisp(code: str, _f: Optional[FrameType] = None) -> str:
    _f = _f or top_level()
    with io.StringIO() as buf, redirect_stdout(buf):
        # pylint: disable=eval-used
        print_elisp(eval(code, _f.f_locals | _f.f_globals))
        return buf.getvalue().strip()

def translate(code: str, _f: Optional[FrameType] = None, use_in_expr: bool = False) -> Any:
    _f = _f or sys._getframe().f_back
    parsed = ast.parse(code, mode="exec").body
    in_expr = try_in_expr(parsed)
    if use_in_expr and in_expr:
        (left, right) = in_expr
        out = to_elisp(ast.unparse(right), _f)
        nc = f"print('''{out}''')\n'select'"
        return ast.parse(nc).body
    elif has_return(parsed):
        r = tr_returns(parsed)
        assert isinstance(r, list)
        return wrap_return(r)
    else:
        return parsed

class EvalResult(TypedDict):
    res: str
    binds: Dict[str, str]
    out: str
    err: Optional[str]

def eval_code(_code: str, _env: Dict[str, Any] = {}) -> EvalResult:
    _res = "unset"
    binds = {}
    out = ""
    err: Optional[str] = None
    _f = _env.get("frame", sys._getframe().f_back)
    if "fname" in _env:
        _f.f_globals["__file__"] = _env["fname"]
    try:
        _code = _code or slurp(_env["code"])
        new_code = translate(_code, _f, _env.get("use-in-expr", False))
        (*butlast, last) = new_code
        _locals = {}
        locals_1 = _locals
        locals_2 = locals_1.copy()
        locals_globals = _f.f_locals | _f.f_globals
        if "debug" in _env:
            print(f"{ast.unparse(last)=}")
        with io.StringIO() as buf, redirect_stdout(buf):
            if butlast:
                # pylint: disable=exec-used
                exec(ast.unparse(butlast), locals_globals, locals_2)
                for bind in [k for k in locals_2.keys() if k not in locals_1.keys()]:
                    _f.f_globals[bind] = locals_2[bind]
            try:
                # pylint: disable=eval-used
                _res = eval(ast.unparse(last), locals_globals, locals_2)
            except SyntaxError:
                locals_1 = _locals
                locals_2 = locals_1.copy()
                exec(ast.unparse(last), locals_globals, locals_2)
            out = buf.getvalue().strip()
        binds1 = [k for k in locals_2.keys() if k not in locals_1.keys()]
        for bind in binds1:
            _f.f_globals[bind] = locals_2[bind]
        binds2 = [bind for bind in binds1 if bind not in ["__res__", "__return__"]]
        print_fn = cast(Callable[..., str], to_str if _env.get("echo") else str)
        binds = {bind: print_fn(locals_2[bind]) for bind in binds2}
    # except RuntimeError as e:
    #     if str(e) == "break":
    #         pm()
    #     else:
    #         raise
    # pylint: disable=broad-except
    except Exception as e:
        err = f"{e.__class__.__name__}: {e}\n{e.__dict__}"
        _f.f_globals["e"] = e
        locs = e.__traceback__.tb_frame.f_locals.get("locals_2", {})
        for bind in locs:
            _f.f_globals[bind] = locs[bind]
    return {
        "res": to_str(_res) if _env.get("echo") else repr(_res),
        "binds": binds,
        "out": out,
        "err": err
    }

def eval_to_json(code: str, env: Dict[str, Any] = {}) -> None:
    try:
        env["frame"] = sys._getframe().f_back
        s = json.dumps(eval_code(code, env))
        print(s)
    # pylint: disable=broad-except
    except Exception as e:
        print(json.dumps({
            "res": None,
            "binds": {},
            "out": "",
            "err": str(e)}))

def find_module(fname: str) -> Optional[ModuleType]:
    for (name, module) in sys.modules.items():
        if getattr(module, "__file__", None) == fname:
            return module
    return None

def generate_import(code_fname: str, buffer_fname: str) -> None:
    code = slurp(code_fname)
    parsed = ast.parse(code).body[0]
    if isinstance(parsed, ast.FunctionDef):
        name = parsed.name
    module = find_module(buffer_fname)
    assert module
    print(f"from {module.__name__} import {name}")
