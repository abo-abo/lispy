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

import inspect
import re
import platform
try:
    import jedi
except:
    print ("failed to load jedi")

def arglist_retrieve_java (method):
    name = method.__name__
    if hasattr (method, "argslist"):
        # uses only the first args list...
        args = [x.__name__ for x in method.argslist[0].args]
    else:
        methods = eval ("method.__self__.class.getDeclaredMethods ()")
        methods_by_name = [m for m in methods if m.getName () == name]
        assert len (methods_by_name) == 1, "expected only a single method by name %s" % name
        meta = methods_by_name[0]
        args = [str (par.getType ().__name__ + " " + par.getName ()) for par in meta.getParameters ()]
    return inspect.ArgSpec (args, None, None, None)

def arglist_retrieve (sym):
    try:
        if hasattr(inspect, "getfullargspec"):
            res = inspect.getfullargspec (sym)
            return inspect.ArgSpec (args = res.args,
                                    varargs = res.varargs,
                                    defaults = res.defaults,
                                    keywords = res.kwonlydefaults)
        else:
            return inspect.getargspec (sym)
    except TypeError as er:
        if (re.search ("is not a Python function$", er.message)
            and platform.system () == "Java"):
            if inspect.isclass (sym):
                return arglist_retrieve (sym.__init__)
            elif hasattr (sym, "argslist") or \
                 hasattr (sym, "__self__") and hasattr (sym.__self__, "class"):
                return arglist_retrieve_java (sym)
            else:
                print (er.message)
        else:
            print (er.message)

def format_arg (arg_pair):
    name, default_value = arg_pair
    if default_value:
        return name + " = " + default_value
    else:
        return name

def delete (element, lst):
    return [x for x in lst if x != element]

def mapcar (func, lst):
    """Compatibility function for Python3.

    In Python2 `map' returns a list, as expected.  But in Python3
    `map' returns a map object that can be converted to a list.
    """
    return list (map (func, lst))

def arglist (sym):
    arg_info = arglist_retrieve (sym)
    if "self" in arg_info.args:
        arg_info.args.remove ("self")
    if arg_info.defaults:
        defaults = [None] * (len (arg_info.args) - len (arg_info.defaults)) + \
                   mapcar (repr, arg_info.defaults)
        args = mapcar (format_arg, zip (arg_info.args, defaults))
    else:
        args = arg_info.args
        if arg_info.varargs:
            args += arg_info.varargs
    if arg_info.keywords:
        if type (arg_info.keywords) is dict:
            for k, v in arg_info.keywords.items ():
                args.append ("%s = %s" % (k, v))
        else:
            args.append ("**" + arg_info.keywords)
    return args

def arglist_jedi (line, column, filename):
    script = jedi.Script(None, line, column, filename)
    defs = script.goto_definitions ()
    if len (defs) == 0:
        raise TypeError ("0 definitions found")
    elif len (defs) > 1:
        raise TypeError (">1 definitions found")
    else:
        return delete ('', mapcar (lambda x: str (x.name), defs[0].params))

def is_assignment (code):
    ops = ast.parse (code).body
    return len (ops) == 1 and type (ops[0]) is ast.Assign
