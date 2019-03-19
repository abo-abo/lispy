# lispy-python.py --- lispy support for Python.

# Copyright (C) 2016 Oleh Krehel

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
import sys
import inspect
import re
import platform
import shlex
import types
try:
    import jedi
except:
    print("failed to load jedi")

#* Classes
class Stack:
    def __init__(self, tb):
        self.stack = []
        while tb:
            self.stack.append((
                tb.tb_frame.f_code.co_filename,
                tb.tb_frame.f_code.co_firstlineno,
                tb.tb_frame))
            tb = tb.tb_next
        self.stack_top = len(self.stack) - 1
        self.set_frame(self.stack_top)

    def frame_string(self, i):
        f = self.stack[i][2]
        res = "  File \"%s\", line %d, Frame [%d/%d]:" % \
           (f.f_code.co_filename, f.f_lineno, i, self.stack_top)
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
        f = self.stack[i][2]
        self.stack_idx = i
        tf = top_level()
        tf.f_globals["lnames"] = f.f_locals.keys()
        for (k, v) in f.f_locals.items():
            tf.f_globals[k] = v
        for (k, v) in f.f_globals.items():
            tf.f_globals[k] = v

        print(self.frame_string(self.stack_idx))

    def up(self, delta = 1):
        if self.stack_idx <= 0:
            print("top frame already")
            print(self.frame_string(self.stack_idx))
        else:
            self.stack_idx = max(self.stack_idx - delta, 0)
            self.set_frame(self.stack_idx)

    def down(self, delta = 1):
        if self.stack_idx >= self.stack_top:
            print("bottom frame already")
            print(self.frame_string(self.stack_idx))
        else:
            self.stack_idx = min(self.stack_idx + delta, self.stack_top)
            self.set_frame(self.stack_idx)

class Autocall:
    def __init__(self, f):
        self.f = f

    def __repr__(self):
        self.f()
        return ""

#* Functions
def arglist_retrieve_java(method):
    name = method.__name__
    if hasattr(method, "argslist"):
        # uses only the first args list...
        args = [x.__name__ for x in method.argslist[0].args]
    else:
        methods = eval("method.__self__.class.getDeclaredMethods()")
        methods_by_name = [m for m in methods if m.getName() == name]
        assert len(methods_by_name) == 1, "expected only a single method by name %s" % name
        meta = methods_by_name[0]
        args = [str(par.getType().__name__ + " " + par.getName()) for par in meta.getParameters()]
    return inspect.ArgSpec(args, None, None, None)

def arglist_retrieve(sym):
    try:
        if hasattr(inspect, "getfullargspec"):
            res = inspect.getfullargspec(sym)
            return inspect.ArgSpec(args = res.args,
                                    varargs = res.varargs,
                                    defaults = res.defaults,
                                    keywords = res.kwonlydefaults)
        else:
            return inspect.getargspec(sym)
    except TypeError as er:
        if(re.search("is not a Python function$", er.message)
            and platform.system() == "Java"):
            if inspect.isclass(sym):
                return arglist_retrieve(sym.__init__)
            elif hasattr(sym, "argslist") or \
                 hasattr(sym, "__self__") and hasattr(sym.__self__, "class"):
                return arglist_retrieve_java(sym)
            else:
                print(er.message)
        else:
            print(er.message)

def format_arg(arg_pair):
    name, default_value = arg_pair
    if default_value:
        return name + " = " + default_value
    else:
        return name

def delete(element, lst):
    return [x for x in lst if x != element]

def mapcar(func, lst):
    """Compatibility function for Python3.

    In Python2 `map' returns a list, as expected.  But in Python3
    `map' returns a map object that can be converted to a list.
    """
    return list(map(func, lst))

def arglist(sym):
    arg_info = arglist_retrieve(sym)
    if "self" in arg_info.args:
        arg_info.args.remove("self")
    if arg_info.defaults:
        defaults = [None] *(len(arg_info.args) - len(arg_info.defaults)) + \
                   mapcar(repr, arg_info.defaults)
        args = mapcar(format_arg, zip(arg_info.args, defaults))
    else:
        args = arg_info.args
        if arg_info.varargs:
            args += arg_info.varargs
    if arg_info.keywords:
        if type(arg_info.keywords) is dict:
            for k, v in arg_info.keywords.items():
                args.append("%s = %s" %(k, v))
        else:
            args.append("**" + arg_info.keywords)
    return args

def arglist_jedi(line, column, filename):
    script = jedi.Script(None, line, column, filename)
    defs = script.goto_definitions()
    if len(defs) == 0:
        raise TypeError("0 definitions found")
    elif len(defs) > 1:
        raise TypeError(">1 definitions found")
    else:
        return delete('', mapcar(lambda x: str(x.name), defs[0].params))

def is_assignment(code):
    ops = ast.parse(code).body
    return len(ops) == 1 and type(ops[0]) is ast.Assign

def top_level():
    """Return the topmost frame."""
    f = sys._getframe()
    while f.f_back:
        f = f.f_back
    return f

def list_step(varname, lst):
    f_globals = top_level().f_globals
    try:
        val = f_globals[varname]
        i =(lst.index(val) + 1) % len(lst)
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

def rebind(cls_name, fun_name):
    """Rebind FUN_NAME in all top level instances of CLS_NAME.

    Modifying a method is two-step:
    1. eval the method as if it's a free top-level function,
    2. modify all instances of the class with an adapter to this top-level function.
    """
    for (n, v) in find_global_vars(cls_name):
        print("rebind:", n)
        top_level().f_globals[n].__dict__[fun_name] = types.MethodType(top_level().f_globals[fun_name], v)

def pm():
    """Post mortem: recover the locals and globals from the last traceback."""
    stack = Stack(sys.last_traceback)
    tl = top_level()
    tl.f_globals["up"] = Autocall(stack.up)
    tl.f_globals["dn"] = Autocall(stack.down)
    globals()["stack"] = stack
