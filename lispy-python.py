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
import inspect
import io
import os
import pprint as pp
import re
import shlex
import subprocess
import sys
import types
from ast import AST
from contextlib import redirect_stdout
from typing import List, Dict, Any, Union, Tuple

def sh(cmd):
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
    repr1.maxstring = 100
except:
    pass
try:
    import jedi
except:
    pyenv_version = sh("pyenv global")
    pyversion = ".".join(pyenv_version.split(".")[:-1])
    site_packages = os.path.expanduser(f"~/.pyenv/versions/{pyenv_version}/lib/python{pyversion}/site-packages/")
    sys.path.append(site_packages)
    import jedi

#* Classes
class Stack:
    line_numbers: Dict[Tuple[str, str], int] = {}

    def __init__(self, tb):
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

    def frame_string(self, i):
        (fname, line, f) = self.stack[i]
        res = "  File \"%s\", line %d, Frame [%d/%d] (%s):" % (
            f.f_code.co_filename, line, i, self.stack_top, f.f_code.co_name)
        return res

    def __repr__(self):
        frames = []
        for i in range(self.stack_top + 1):
            s = self.frame_string(i)
            if i == self.stack_idx:
                s += "*"
            frames.append(s)
        return "\n".join(frames)

    def set_frame(self, i):
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

    def up(self, delta=1):
        if self.stack_idx <= 0:
            if self.stack:
                print(self.frame_string(self.stack_idx))
        else:
            self.stack_idx = max(self.stack_idx - delta, 0)
            self.set_frame(self.stack_idx)

    def down(self, delta=1):
        if self.stack_idx >= self.stack_top:
            if self.stack:
                print(self.frame_string(self.stack_idx))
        else:
            self.stack_idx = min(self.stack_idx + delta, self.stack_top)
            self.set_frame(self.stack_idx)

class Autocall:
    def __init__(self, f):
        self.f = f

    def __call__(self, n):
        self.f(n)

    def __repr__(self):
        try:
            self.f()
        except:
            pass
        return ""

#* Functions
def chfile(f):
    tf = top_level()
    tf.f_globals["__file__"] = f
    d = os.path.dirname(f)
    try:
        os.chdir(d)
    except:
        pass

def format_arg(arg_pair):
    name, default_value = arg_pair
    if default_value:
        return name + " = " + default_value
    else:
        return name

def arglist(sym):
    arg_info = inspect.getfullargspec(sym)
    if "self" in arg_info.args:
        arg_info.args.remove("self")
    if arg_info.defaults:
        defaults = (
            [None] * (len(arg_info.args) - len(arg_info.defaults)) +
            [repr(x) for x in arg_info.defaults])
        args = [format_arg(x) for x in zip(arg_info.args, defaults)]
    else:
        args = arg_info.args
        if arg_info.varargs:
            args += arg_info.varargs
    keywords = arg_info.kwonlydefaults
    if keywords:
        if type(keywords) is dict:
            for k, v in keywords.items():
                args.append(f"{k} = {v}")
        else:
            args.append("**" + keywords)
    return args

def print_elisp(obj, end="\n"):
    if hasattr(obj, "_asdict") and obj._asdict is not None:
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
    else:
        if obj:
            if type(obj) is list or type(obj) is tuple:
                print("(", end="")
                for x in obj:
                    print_elisp(x)
                print(")")
            elif type(obj) is str:
                # quote strings?
                # print("\"'" + re.sub("\"", "\\\"", obj) + "'\"", end=" ")
                print('"' + re.sub("\"", "\\\"", obj) + '"', end=" ")
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

def argv(cmd):
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
        top_level().f_globals[n].__dict__[fun_name] = types.MethodType(top_level().f_globals[fun_name], v)
    if fname and line:
        Stack.line_numbers[(fname, qname)] = line

def pm():
    """Post mortem: recover the locals and globals from the last traceback."""
    if hasattr(sys, 'last_traceback'):
        stack = Stack(sys.last_traceback)
    else:
        stack = Stack(sys.exc_info()[2])
    tl = top_level()
    tl.f_globals["up"] = Autocall(stack.up)
    tl.f_globals["dn"] = Autocall(stack.down)
    globals()["stack"] = stack

def pprint(x):
    r1 = repr(x)
    if len(r1) > 1000 and repr1:
        print(repr1.repr(x))
    else:
        if type(x) == collections.OrderedDict:
            print("{" + ",\n ".join([str(k) + ": " + str(v) for (k, v) in x.items()]) + "}")
        else:
            pp.PrettyPrinter(width=200).pprint(x)

def to_str(x):
    with io.StringIO() as buf, redirect_stdout(buf):
        pprint(x)
        return buf.getvalue().strip()

def step_in(fn, *args):
    spec = inspect.getfullargspec(fn)
    f_globals = top_level().f_globals
    for (arg_name, arg_val) in zip(spec.args, args):
        f_globals[arg_name] = arg_val

def step_into_module_maybe(module):
    if isinstance(module, types.FunctionType):
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
        for x in set(list(o.__dict__.keys()) + list(type(o).__dict__.keys())):
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

def reload():
    import importlib.util
    spec = importlib.util.spec_from_file_location('lispy-python', __file__)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    top_level().f_globals["lp"] = mod

def reload_module(fname):
    import importlib
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

def wrap_return(parsed: Expr) -> Expr:
    return [
        ast.FunctionDef(
            name="__res__",
            body=parsed,
            decorator_list=[],
            args=[],
            lineno=0,
            col_offset=0),
        ast.Expr(
            ast_call(
                ast.Attribute(value=ast_call("globals"), attr="update"),
                args=[ast_call("__res__")]))]

def translate_assign(p: Expr) -> Expr:
    if isinstance(p, list):
        if isinstance(p[-1], ast.Assign):
            return [*p, ast.Expr(
                ast_call("print", p[-1].targets))]
        elif isinstance(p[-1], ast.Expr):
            return [*p[:-1], ast.Expr(
                ast_call("print", [p[-1]]))]
        else:
            return [*p, ast.Expr(
                ast_call("print", [ast.Constant("(ok)")]))]
    return p

def tr_print_last_expr(p: Expr) -> Expr:
    if isinstance(p, list):
        if isinstance(p[-1], ast.Expr):
            return [*p[:-1], ast.Expr(ast_call("print", [p[-1]]))]
    return p

def translate(code: str) -> Any:
    parsed = ast.parse(code, mode="exec").body
    if has_return(parsed):
        parsed_1 =  wrap_return(tr_returns(parsed))
        return [*parsed_1, ast.Expr(ast_call("print", [ast.Name("__return__")]))]
    else:
        # parsed_1 = translate_assign(parsed)
        return tr_print_last_expr(parsed)

def eval_code(_code: str, env: Dict[str, Any] = {}) -> None:
    _code = _code or slurp(env["code"])
    new_code = ast.unparse(translate(_code))
    # print(f"{new_code=}")
    locals_1 = locals()
    locals_2 = locals_1.copy()
    # pylint: disable=exec-used
    exec(new_code, top_level().f_globals | {"__file__": env.get("fname")}, locals_2)
    binds = [k for k in locals_2.keys() if k not in locals_1.keys()]
    for bind in binds:
        top_level().f_globals[bind] = locals_2[bind]

    binds_to_print = binds if env.get("echo") else binds[-1:]
    for bind in binds_to_print:
        if bind != "__res__":
            v = to_str(locals_2[bind])
            print(f"{bind} = {v}")
