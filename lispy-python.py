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
import jedi

def arglist_retrieve (sym):
    try:
        return inspect.getargspec (sym.__init__)
    except TypeError:
        return inspect.getargspec (sym)

def format_arg (arg_pair):
    name, default_value = arg_pair
    if default_value:
        return name + " = " + default_value
    else:
        return name

def delete (element, lst):
    return [x for x in lst if x != element]

def arglist (sym, filename = None, line = None, column = None):
    try:
        arg_info = arglist_retrieve (sym)
        if "self" in arg_info.args:
            arg_info.args.remove ("self")
        if arg_info.defaults:
            defaults = [None] * (len (arg_info.args) - len (arg_info.defaults)) + \
                       map (repr, arg_info.defaults)
            args = map (format_arg, zip (arg_info.args, defaults))
        else:
            args = arg_info.args
        if arg_info.keywords:
            args.append ("**" + arg_info.keywords)
        return args
    except TypeError:
        script = jedi.Script(None, line, column, filename)
        defs = script.goto_definitions ()
        if len (defs) == 0:
            raise TypeError ("0 definitions found")
        elif len (defs) > 1:
            raise TypeError (">1 definitions found")
        else:
            return delete ('', map (lambda (x): x.description, defs[0].params))
