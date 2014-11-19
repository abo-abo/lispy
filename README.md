# Introduction

This package implements various commands for navigating and editing
Lisp code. It's influenced by Paredit and re-implements most of its features
in order to provide faster and more intuitive key bindings.

## Relation to vi

Here's a quote from Wikipedia on how vi works, in case you don't know:

> vi is a modal editor: it operates in either insert mode (where typed
> text becomes part of the document) or normal mode (where keystrokes
> are interpreted as commands that control the edit session). For
> example, typing i while in normal mode switches the editor to insert
> mode, but typing i again at this point places an "i" character in
> the document. From insert mode, pressing ESC switches the editor
> back to normal mode.

Here's an illustration of Emacs, vi and lispy bindings for inserting a
char and calling a command:

    |                   | insert "j" | forward-list |
    |-------------------+------------+--------------|
    | Emacs             | j          | C-M-n        |
    | vi in insert mode | j          | impossible   |
    | vi in normal mode | impossible | j            |
    | lispy             | j          | j            |

Advantages/disadvantages:

- Emacs can do both things without switching modes, but the command
  binding are long
- vi has short command bindings, but you have to switch modes to do both things
- lispy has short command bindings and doesn't need to switch modes

### How does lispy work?

Of course it's not magic, lispy needs to have normal/insert mode to
perform both functions with `j`.  The difference from vi is that the
mode is *explicit* instead of *implicit*, it's determined by the point
position or the region state:

- you're in normal mode when the point is before/after paren or the
  region is active
- otherwise it's in insert mode

But if you ask: "what if I want to insert when the point is
before/after paren or the region is active?". The answer is that you
don't want to because of how LISP code is structured.

You don't want to write this:

    j(progn
       (forward-char 1))k

## Features

- Basic navigation by-list and by-region with `h`, `j`, `k`, `l`.
- Paredit transformations, callable by plain letters, such as `>` for
  `slurp`, `r` for `raise`, and `C` for `convolute`.
- [IDE-like features](#ide-like-features) for Elisp, Clojure, Scheme and Common Lisp:
    - `e` evals
    - `E` evals and inserts
    - `g` jumps to tag with semantic
    - `M-.` jumps to symbol, `.` jumps back
    - `C-1` shows documentation in an overlay
    - `C-2` shows arguments in an overlay

- Code manipulation:
    - `i` prettifies code (remove extra space, hanging parens ...)
    - `xi` transforms `cond` expression to equivalent `if` expressions
    - `xc` transforms `if` expressions to an equivalent `cond` expression
    - `xf` flattens function or macro call (extract body and substitute arguments)
    - `xl` turns current `defun` into a `lambda`
    - `xd` turns current `lambda` into a `defun`
    - `O` formats the code into one line
    - `M` formats the code into multiple lines

- Misc. bindings:
    - outlines (`J`, `K`, `I`)
    - narrow/widen (`N`, `W`)
    - `ediff` (`b`, `B`)
    - `ert` (`T`)
    - `edebug` (`x e`)

## Function reference
Available at http://abo-abo.github.io/lispy/.


## Installation instructions
It's easiest to install this from [MELPA](http://melpa.milkbox.net/).
Here's a minimal MELPA configuration for your `~/.emacs`:

    (package-initialize)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

Afterwards, do M-x `package-install`, type in `lispy` and RET.
You can now call M-x `lispy-mode` for any buffer with a Lisp dialect source.
To have `lispy-mode` activated automatically, use something like this:

    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

## Special positions and key bindings

Due to the structure of Lisp syntax it's very rare for the
programmer to want to insert characters right before "(" or right
after ")". Thus unprefixed printable characters can be used to call
commands when the point is at one of these locations, which are
further referred to as special. See also [Regions are special too](#regions-are-special-too).

Conveniently, when located at special position it's very clear to
which sexp the list-manipulating command will be applied to, what
the result be and where the point should end up afterwards.  You
can enhance this effect with `show-paren-mode` or similar.

Here's an illustration to this effect, with `lispy-clone` ("*"
represents the point):

    |--------------------+-----+--------------------|
    | before             | key | after              |
    |--------------------+-----+--------------------|
    |  (looking-at "(")* |  c  |  (looking-at "(")  |
    |                    |     |  (looking-at "(")* |
    |--------------------+-----+--------------------|
    | *(looking-at "(")  |  c  | *(looking-at "(")  |
    |                    |     |  (looking-at "(")  |
    |--------------------+-----+--------------------|

## Digit keys

When special, the digit keys call `digit-argument` which is very
useful since most Lispy commands accept a numeric argument.
For instance, "3c" is equivalent to "ccc" (clone sexp 3 times), and
"4j" is equivalent to "jjjj" (move point 4 sexps down).  Some useful
applications are "9l" and "9a" - they exit list forwards and
backwards respectively at most 9 times which makes them effectively
equivalent to `end-of-defun` and `beginning-of-defun`.

## How to get into special

To move the point into a special position, use:

    "]" - calls `lispy-forward`
    "[" - calls `lispy-backward`
    "C-3" - calls `lispy-out-forward` (exit current list forwards)
    ")" - calls `lispy-out-forward-nostring` (exit current list
          forwards, but self-insert in strings and comments)

These are the few Lispy commands that don't care whether the point
is special or not. Other such bindings are "DEL", "C-d", "C-k".

## How to get out of special

To get out of the special position, you can use any of the good-old
navigational commands such as "C-f" or "C-n".
Additionally "SPC" will break out of special to get around the
situation when you have the point between open parens like this
"(|(" and want to start inserting. "SPC" will change the code to
this: "(| (".

## Command reference

A lot of Lispy commands come in pairs: one reverses the other.
Some examples are:

    |-----+--------------------------+------------+-------------------|
    | key | command                  | key        | command           |
    |-----+--------------------------+------------+-------------------|
    | j   | `lispy-down'             | k          | `lispy-up'        |
    | s   | `lispy-move-down'        | w          | `lispy-move-up'   |
    | >   | `lispy-slurp'            | <          | `lispy-barf'      |
    | c   | `lispy-clone'            | C-d or DEL |                   |
    | C   | `lispy-convolute'        | C          | reverses itself   |
    | d   | `lispy-different'        | d          | reverses itself   |
    | M-j | `lispy-split'            | +          | `lispy-join'      |
    | O   | `lispy-oneline'          | M          | `lispy-multiline' |
    | S   | `lispy-stringify'        | C-u "      | `lispy-quotes'    |
    | ;   | `lispy-comment'          | C-u ;      | `lispy-comment'   |
    | xi  | `lispy-to-ifs'           | xc         | `lispy-to-cond'   |
    |-----+--------------------------+------------+-------------------|

Here's a list of commands for inserting pairs:

    |-----+------------------------------------|
    | key | command                            |
    |-----+------------------------------------|
    |  (  | `lispy-parens'                     |
    |  {  | `lispy-braces'                     |
    |  }  | `lispy-brackets'                   |
    |  "  | `lispy-quotes'                     |
    |-----+------------------------------------|


Here's a list of modified insertion commands that handle whitespace
in addition to self-inserting:

    |-----+------------------------------------|
    | key | command                            |
    |-----+------------------------------------|
    | SPC | `lispy-space'                      |
    |  :  | `lispy-colon'                      |
    |  ^  | `lispy-hat'                        |
    | C-m | `lispy-newline-and-indent'         |
    |-----+------------------------------------|

Among other cool commands are:

    |-------+----------------------------------|
    | key   | command                          |
    |-------+----------------------------------|
    | f     | `lispy-flow'                     |
    | u     | `lispy-undo'                     |
    | m     | `lispy-mark-list'                |
    | l     | `lispy-out-forward'              |
    | a     | `lispy-out-backward'             |
    | /     | `lispy-splice'                   |
    | i     | `indent-sexp'                    |
    | r     | `lispy-raise'                    |
    | R     | `lispy-raise-some'               |
    | J     | `lispy-outline-next'             |
    | K     | `lispy-outline-prev'             |
    | q     | `lispy-ace-paren'                |
    | h     | `lispy-ace-symbol'               |
    | Q     | `lispy-ace-char'                 |
    | N     | `lispy-normalize'                |
    | C-7   | `lispy-cursor-down'              |
    | C-8   | `lispy-parens-down'              |
    | C-e   | `lispy-move-end-of-line'         |
    | C-k   | `lispy-kill'                     |
    | C-y   | `lispy-yank'                     |
    | C-d   | `lispy-delete'                   |
    | DEL   | `lispy-delete-backward'          |
    | C-,   | `lispy-kill-at-point'            |
    | C-M-, | `lispy-mark'                     |
    | M-m   | `lispy-mark-symbol'              |
    | v     | `lispy-view'                     |
    | n     | `lispy-new-copy'                 |
    | xd    | `lispy-to-defun'                 |
    | xl    | `lispy-to-lambda'                |
    | xm    | `lispy-cursor-ace'               |
    | xe    | `edebug-defun'                   |
    |-------+----------------------------------|

Also, IDE-like commands ([details here](#ide-like-features)):

    |-----+------------------------------------|
    | C-1 | `lispy-describe-inline`            |
    | C-2 | `lispy-arglist-inline`             |
    | e   | `lispy-eval`                       |
    | E   | `lispy-eval-and-insert`            |
    | D   | `lispy-describe`                   |
    | g   | `lispy-goto`                       |
    | G   | `lispy-goto-local`                 |
    | F   | `lispy-follow`                     |
    |-----+------------------------------------|

Most special commands will leave the point special after they're
done.  This allows to chain them as well as apply them
continuously by holding the key.  Some useful hold-able keys are
"jkopf<>cws;".
Not so useful, but fun is "/": start it from "|(" position and hold
until all your Lisp code is turned into Python :).

## Navigating within a top-level list with `ace-jump`

If you have `ace-jump-mode` installed, you can use "q" to jump
around a top-level list (usually a `defun`).
You can usually get away with typing just one lower case char to navigate
and the position remains special.
And since the ordering always starts from `a`, "qa" is another synonym
for `beginning-of-defun`.

## Regions are special too
Sometimes the expression that you want to operate on isn't bounded by parens.
In that case you can mark it with a region and operate on that.

### Ways to activate region
While in special:
- Mark a sexp with `lispy-mark-list` "m".
- Mark a symbol within sexp with `lispy-ace-symbol` "h".

While not in special:
- `set-mark-command` "C-SPC".
- Mark a symbol at point with `lispy-mark-symbol` "M-m".
- Mark containing expression (list or string or comment) with `lispy-mark` "C-M-,".

### Ways to extend region
To extend the region, use a combination of
- `lispy-down` "j"
  Move one sexp forward. Use prefix argument to move several sexps forward.
- `lispy-up` "k"
  Move one sexp backward. Use prefix argument to move several sexps backward.
- `lispy-different` "d"
  Move to the different side of the region.

### Commands that operate on region
- `lispy-clone` "c"
  Clone region and keep it active.
- `lispy-move-down` "s"
  Move region one sexp down.
- `lispy-move-up` "w".
  Move region one sexp up.
- `lispy-undo` "u"
  Deactivate region and undo.
- `lispy-mark-list` "m"
  Deactivate region.
- `lispy-out-forward` "l"
  Move region out of current sexp forwards.
- `lispy-out-backward` "a"
  Move region out of current sexp backwards.
- `lispy-teleport` "t"
  Move region inside the sexp you select with `lispy-ace-paren`.
- `lispy-slurp` ">"
  Move region inside next sexp ("a" will reverse this).
- `lispy-barf` "<"
  Move region inside previous sexp ("l" will reverse this).
- `lispy-convolute` "C"__
Exchange the order of application of two sexps that contain point.
- `lispy-new-copy` "n"
Copy region as kill without deactivating the region.
Useful to search for currently marked symbol with "n g C-y".


## IDE-like features
These features are specific to the Lisp dialect used.
Currently Elisp and Clojure (via `cider`) are supported.
There's also basic evaluation support for Scheme (via `geiser`)
and Common lisp (via `slime`).
### `lispy-describe-inline`
Bound to `C-1`. Show doc for current function inline.

`<f1> f` is fine, but the extra buffer, and having to navigate to a symbol
is tiresome. `C-1` toggles on/off the inline doc for current function.
No extra buffer necessary:

![screenshot](https://raw.github.com/abo-abo/lispy/master/doc/doc-1.png)

Here's how it looks for Clojure:

![screenshot](https://raw.github.com/abo-abo/lispy/master/doc/doc-2.png)

### `lispy-arglist-inline`
Bound to `C-2`. Show arguments for current function inline.

`eldoc-mode` is cool, but it shows you arguments *over there* and
you're writing *over here*!. No problem, `C-2` fixes that:

![screenshot](https://raw.github.com/abo-abo/lispy/master/doc/arglist-1.png)

In a recent version normal, &optional and &rest arguments have each
a different face:

![screenshot](https://raw.github.com/abo-abo/lispy/master/doc/arglist-2.png)

Here's how it looks for Clojure:

![screenshot](https://raw.github.com/abo-abo/lispy/master/doc/arglist-3.png)

### `lispy-eval`
Bound to `e` while in special. Eval current expression.

This is just a convenience binding. Works from the beginning of list
as well.
### `lispy-eval-and-insert`
Bound to `E` while in special. Eval and insert current expression.

This is just a convenience binding. Works from the beginning of list
as well.

### `lispy-follow`
Bound to `F` while in special. Follow to definition of current function.
While in Clojure, if can't resolve the symbol in current namespace,
searches for it in all loaded namespaces.

### `lispy-goto`
Bound to `g` while in special. Use `helm` to select a top-level symbol
to jump to in current directory.


Works out of the box for Elisp, Scheme and Common Lisp.
[clojure-semantic](https://github.com/kototama/clojure-semantic)
is required for Clojure.

## `lispy-goto-local`
Bound to `G` while in special. Like `lispy-goto`, but works only on current file.

# Screencasts

## screencast 1

This one isn't the best quality, so it's not [inline](https://raw.github.com/abo-abo/lispy/master/doc/screencast-1.gif).

## screencast 2

This one is also available [on vimeo](https://vimeo.com/85831418)
and for [download](https://raw.github.com/abo-abo/lispy/master/doc/screencast-2.ogv).

![screencast-2.gif](https://raw.github.com/abo-abo/lispy/master/doc/screencast-2.gif)

## screencast 3

This demonstrates mostly `lispy-comment` (";") and `special-undo` ("u").

![screencast-3.gif](https://raw.github.com/abo-abo/lispy/master/doc/screencast-3.gif)

## screencast 4
Synopsis: find a function and make it echo its doc-string.

1. Use `lispy-goto` "g" to find the function.
2. Use `lispy-ace-symbol` "h" to mark its doc-string.
3. Use `lispy-clone` "c" to clone region. Note that the region doesn't disappear.
4. Use `lispy-parens` "(" to wrap region.
5. Insert "message " and use `lispy-forward` "]" to exit the list and move into special.
6. Use `lispy-mark-list` "m" to mark current list so that we can move it later.
7. Use `lispy-move-down` "s" to move marked sexp all the way down.
8. Use `lispy-forward` "]" twice to exit defun.
9. Use `lispy-eval` "e" to eval our changes.
10. Use `lispy-undo` "u" to see that the change is now in effect.

![screencast-4.gif](https://raw.github.com/abo-abo/lispy/master/doc/screencast-4.gif)

## screencast 5
Synopsis:

- press "O" to fit the current list on one line
- press "M" to fit the current list on multiple lines

Note that there's multiple ways to implement the rules for "M".  It's
maybe not sophisticated enough to save the code formatting verbatim,
but it sure does help making `macroexpand` output readable.

![oneline-multiline.gif](https://raw.github.com/abo-abo/lispy/master/doc/oneline-multiline.gif)
