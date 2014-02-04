# Introduction

This package implements various commands for navigating and editing
Lisp code. It's influenced by Paredit and re-implements most of its features
in order to provide faster and more intuitive key bindings.

It also provides some [IDE-like features](#ide-like-features) for Elisp and Clojure.

## Special positions and key bindings

Due to the structure of Lisp syntax it's very rare for the
programmer to want to insert characters right before "(" or right
after ")". Thus unprefixed printable characters can be used to call
commands when the point is at one of these locations, which are
further referred to as special.

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
    | j   | `lispy-down`             | k          | `lispy-up`        |
    | s   | `lispy-move-down`        | w          | `lispy-move-up`   |
    | o   | `lispy-counterclockwise` | p          | `lispy-clockwise` |
    | >   | `lispy-slurp`            | <          | `lispy-barf`      |
    | c   | `lispy-clone`            | C-d or DEL |                   |
    | C   | `lispy-convolute`        | C          | reverses itself   |
    | d   | `lispy-different`        | d          | reverses itself   |
    | M-j | `lispy-split`            | +          | `lispy-join`      |
    | O   | `lispy-oneline`          | M          | `lispy-multiline` |
    | S   | `lispy-stringify`        | C-u "      | `lispy-quotes`    |
    | ;   | `lispy-comment`          | C-u ;      | `lispy-comment`   |
    |-----+--------------------------+------------+-------------------|

Among other cool commands are:

    |-------+------------------------------------|
    | key   | command                            |
    |-------+------------------------------------|
    | f     | `lispy-flow`                       |
    | u     | `undo`                             |
    | m     | `lispy-mark-list`                  |
    | l     | `lispy-out-forward`                |
    | a     | `lispy-out-backward`               |
    | /     | `lispy-splice`                     |
    | i     | `indent-sexp`                      |
    | r     | `lispy-raise`                      |
    | R     | `lispy-raise-some`                 |
    | J     | `outline-next-visible-heading`     |
    | K     | `outline-previous-visible-heading` |
    | q     | `lispy-ace-paren`                  |
    | h     | `lispy-ace-symbol`                 |
    | Q     | `lispy-ace-char`                   |
    | N     | `lispy-normalize`                  |
    | M-m   | `lispy-mark-symbol`                |
    | M-o   | `lispy-string-one-line`            |
    | C-,   | `lispy-kill-at-point`              |
    | C-M-, | `lispy-kill-at-point`              |
    | C-e   | `lispy-move-end-of-line`           |
    | t     | `lispy-teleport`                   |
    |-------+------------------------------------|

Also, IDE-like commands ([details here](#ide-like-features)):

    |-----+------------------------------------|
    | C-1 | `lispy-describe-inline`            |
    | C-2 | `lispy-arglist-inline`             |
    | e   | `lispy-eval`                       |
    | E   | `lispy-eval-and-insert`            |
    | D   | `lispy-describe`                   |
    | g   | `lispy-goto`                       |
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

## IDE-like features
These features are specific to the Lisp dialect used.
Currently Elisp and Clojure (via `cider`) are supported.
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
to jump to in current file.

Works out of the box for Elisp.
[clojure-semantic](https://github.com/kototama/clojure-semantic)
is required for Clojure.

# Screencasts

## screencast 1

This one isn't the best quality, so it's not [inline](https://raw.github.com/abo-abo/lispy/master/doc/screencast-1.gif).

## screencast 2

This one is also available [on vimeo](https://vimeo.com/85831418)
and for [download](https://raw.github.com/abo-abo/lispy/master/doc/screencast-2.ogv).

![screencast-2.gif](https://raw.github.com/abo-abo/lispy/master/doc/screencast-2.gif)



