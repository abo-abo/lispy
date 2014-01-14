# Introduction

This package implements various commands for navigating and editing
Lisp code. It's influenced by Paredit and re-implements most of its features
in order to provide faster and more intuitive key bindings.

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
convenient since most Lispy commands accept a numeric argument.
For instance, "3c" is equivalent to "ccc" (clone sexp 3 times), and
"4j" is equivalent to "jjjj" (move point 4 sexps down).  Some useful
applications are "9l" and "9a" - they exit list forwards and
backwards respectively at most 9 times which makes them effectively
equivalent to `end-of-defun` and `beginning-of-defun`.

## How to get into special

To move the point into a special position, use:

    "]" - calls `lispy-forward'
    "[" - calls `lispy-backward'
    "C-3" - calls `lispy-out-forward' (exit current list forwards)
    ")" - calls `lispy-out-forward-nostring' (exit current list
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
    |-----+--------------------------+------------+-------------------|

Among other cool commands are:

    |-----+------------------------------------|
    | key | command                            |
    |-----+------------------------------------|
    | f   | `lispy-flow`                       |
    | u   | `undo`                             |
    | e   | `lispy-eval`                       |
    | m   | `lispy-mark`                       |
    | ;   | `lispy-comment`                    |
    | l   | `lispy-out-forward`                |
    | a   | `lispy-out-backward`               |
    | E   | `lispy-eval-and-insert`            |
    | /   | `lispy-splice`                     |
    | i   | `indent-sexp`                      |
    | r   | `lispy-raise`                      |
    | R   | `lispy-raise-some`                 |
    | J   | `outline-next-visible-heading`     |
    | K   | `outline-previous-visible-heading` |
    | g   | `lispy-goto`                       |
    | q   | `lispy-ace-paren`                  |
    | Q   | `lispy-ace-char`                   |
    | S   | `lispy-stringify`                  |
    | D   | `lispy-describe`                   |
    | F   | `lispy-follow`                     |
    |-----+------------------------------------|

Most special commands will leave the point special after they're
done.  This allows to chain them as well as apply them
continuously by holding the key.  Some useful holdable keys are
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
