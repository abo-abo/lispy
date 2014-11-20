<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
    - [Relation to vi](#relation-to-vi)
    - [Features](#features)
    - [Function reference](#function-reference)
- [Getting Started](#getting-started)
    - [Installation instructions](#installation-instructions)
    - [Operating on lists](#operating-on-lists)
        - [How to get into list-editing mode (special)](#how-to-get-into-list-editing-mode-special)
        - [Digit keys in special](#digit-keys-in-special)
        - [How to get out of special](#how-to-get-out-of-special)
        - [List commands overview](#list-commands-overview)
        - [Navigating within a top-level list with `ace-jump`](#navigating-within-a-top-level-list-with-ace-jump)
    - [Operating on regions](#operating-on-regions)
        - [Ways to activate region](#ways-to-activate-region)
        - [Move region around](#move-region-around)
        - [Switch to other side of the region](#switch-to-other-side-of-the-region)
        - [Grow/shrink region](#growshrink-region)
        - [Commands that operate on region](#commands-that-operate-on-region)
    - [IDE-like features](#ide-like-features)
        - [`lispy-describe-inline`](#lispy-describe-inline)
        - [`lispy-arglist-inline`](#lispy-arglist-inline)
        - [`lispy-eval`](#lispy-eval)
        - [`lispy-eval-and-insert`](#lispy-eval-and-insert)
        - [`lispy-follow`](#lispy-follow)
        - [`lispy-goto`](#lispy-goto)
        - [`lispy-goto-local`](#lispy-goto-local)
- [Screencasts](#screencasts)

<!-- markdown-toc end -->

# Introduction

This package implements a special key binding method and various commands that are
useful for navigating and editing LISP code.
The key binding method is influenced by vi, although this isn't modal editing *per se*.
The list manipulation commands are influenced by and are a superset of Paredit.

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

                  | insert "j" | forward-list
------------------|------------|--------------
Emacs             | j          | C-M-n
vi in insert mode | j          | impossible
vi in normal mode | impossible | j
lispy             | j          | j

Advantages/disadvantages:

- Emacs can both insert and call commands without switching modes (since it has none),
  but the command bindings are long
- vi has short command bindings, but you have to switch modes between inserting and calling commands
- lispy has short command bindings and doesn't need to switch modes

Of course it's not magic, lispy needs to have normal/insert mode to
perform both functions with `j`.  The difference from vi is that the
mode is *explicit* instead of *implicit* - it's determined by the point
position or the region state:

- you're in normal mode when the point is before/after paren or the
  region is active
- otherwise it's in insert mode

But if you ask:

> What if I want to insert when the point is before/after paren or the region is active?

The answer is that because of the LISP syntax you don't want to write this:

    j(progn
       (forward-char 1))k

Also, Emacs does nothing special by default when the region is active
and you press a normal key, so new commands can be called in that situation.

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
Most functions are cataloged and described at http://abo-abo.github.io/lispy/.

# Getting Started
## Installation instructions
It's easiest to install this from [MELPA](http://melpa.milkbox.net/).
Here's a minimal MELPA configuration for your `~/.emacs`:

    (package-initialize)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

Afterwards, do `M-x` `package-install`, type in `lispy` and `RET`.
You can now call `M-x` `lispy-mode` for any buffer with a Lisp dialect source.
To have `lispy-mode` activated automatically, use something like this:

    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

## Operating on lists

### How to get into list-editing mode (special)

The plain keys will call commands when is positioned before paren,
after paren, or the region is active. When one of the first two are
true, I say that the point is special.

When the point is special, it's very clear to which sexp the
list-manipulating command will be applied to, what the result be and
where the point should end up afterwards.  You can enhance this effect
with `show-paren-mode` or similar.

Here's an illustration to this effect, with `lispy-clone` (`|`
represents the point):

before              | key | after
--------------------|-----|------------------------
`(looking-at "(")|` |  c  |  `(looking-at "(")`
                    |     |  `(looking-at "(")|`

before              | key | after
--------------------|-----|------------------------
`|(looking-at "(")` |  c  |  `|(looking-at "(")`
                    |     |   ` (looking-at "(")`

You can use plain Emacs navigation commands to get into special, or you can use
some of the dedicated commands:

Key Binding     | Description
----------------|-----------------------------------------------------------
`］`            | `lispy-forward` - move to the end of the closest list, analogous to `C-M-n` (`forward-list`)
`［`            | `lispy-backward` - move to the start of the closest list, analogous to `C-M-p` (`backward-list`)
`C-3`           | `lispy-out-forward` - exit current list forwards, analogous to `up-list`
`)`             | `lispy-out-forward-nostring` exit current list forwards, but self-insert in strings and comments

These are the few Lispy commands that don't care whether the point is
special or not. Other such bindings are `DEL`, `C-d`, `C-k`.

You want to get into special for manipulating/navigating lists.
If you want to manipulate symbols, use region selection instead.

### Digit keys in special

When special, the digit keys call `digit-argument` which is very
useful since most Lispy commands accept a numeric argument.
For instance, `3c` is equivalent to `ccc` (clone sexp 3 times), and
`4j` is equivalent to `jjjj` (move point 4 sexps down).

Some useful applications are `9l` and `9h` - they exit list forwards
and backwards respectively at most 9 times which makes them
effectively equivalent to `end-of-defun` and `beginning-of-defun`.  Or
you can move to the last sexp of the file with `999j`.

### How to get out of special

To get out of the special position, you can use any of the good-old
navigational commands such as `C-f` or `C-n`.
Additionally `SPC` will break out of special to get around the
situation when you have the point between the open parens like this

    (|(

and want to start inserting; `SPC` will change the code to
this:

    (| (

### List commands overview
The full reference is [here](http://abo-abo.github.io/lispy/).

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

Here's a list of IDE-like commands ([details here](#ide-like-features)):

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
`jkf<>cws;`.
Not so useful, but fun is `/`: start it from `|(` position and hold
until all your Lisp code is turned into Python :).

### Navigating within a top-level list with `ace-jump`

If you have `ace-jump-mode` installed, you can use `q` to jump
around a top-level list (usually a `defun`).
You can usually get away with typing just one lower case char to navigate
and the position remains special.
And since the ordering always starts from "a", `qa` is another synonym
for `beginning-of-defun`.

## Operating on regions
Sometimes the expression that you want to operate on isn't bounded by parens.
In that case you can mark it with a region and operate on that.

### Ways to activate region
While in special:
- Mark a sexp with `m` (`lispy-mark-list`)
- Mark a symbol within sexp `a`(`lispy-ace-symbol`).

While not in special:
- `C-SPC` (`set-mark-command`)
- mark a symbol at point with `M-m` (`lispy-mark-symbol`)
- mark containing expression (list or string or comment) with `C-M-,` (`lispy-mark`)

### Move region around

The arrow keys `j`/`k` will move the region up/down within the current
list.  The actual code will not be changed.

### Switch to other side of the region

Use `d` (`lispy-different`) to switch between different sides of the
region. The side is important since the grow/shrink operations apply
to current side of the region.

### Grow/shrink region

Use a combination of:
- `>` (`lispy-slurp`) - extend by one sexp from the current side. Use digit
  argument to extend by several sexps.
- `<` (`lispy-barf`) - shrink by one sexp from the current side. Use digit
  argument to shrink by several sexps.

The other two arrow keys will mark the parent list of the current region:

- `h` (`lispy-out-backward`) - mark the parent list with the point on the left
- `l` (`lispy-out-forward`) - mark the parent list with the point on the right

To do the reverse of the previous operation, i.e. to mark the first
child of marked list, use `i` (`lispy-tab`).

### Commands that operate on region
- `m` (`lispy-mark-list`) - deactivate region
- `c` (`lispy-clone`) - clone region and keep it active
- `s` (`lispy-move-down`) - move region one sexp down
- `w` (`lispy-move-up`) - move region one sexp up
- `u` (`lispy-undo`) - deactivate region and undo
- `t` (`lispy-teleport`) - move region inside the sexp you select with
  `lispy-ace-paren`.
- `C` (`lispy-convolute`) - exchange the order of application of two
  sexps that contain point.
- `n` (`lispy-new-copy`) - copy region as kill without deactivating
the region.  Useful to search for currently marked symbol with `n g C-y`.


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

![screenshot](https://raw.github.com/abo-abo/lispy/master/doc/arglist-2.png)

As you see, normal, &optional and &rest arguments have each a
different face. Here's how it looks for Clojure:

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

### `lispy-goto-local`
Bound to `G` while in special. Like `lispy-goto`, but works only on current file.

# Screencasts

I've recorded a few gifs that show some features:

- [1-gif](https://raw.github.com/abo-abo/lispy/master/doc/screencast-1.gif),
- [2-vimeo](https://vimeo.com/85831418),
  [2-ogv](https://raw.github.com/abo-abo/lispy/master/doc/screencast-2.ogv),
  [2-gif](https://raw.github.com/abo-abo/lispy/master/doc/screencast-2.gif)
- [3-gif](https://raw.github.com/abo-abo/lispy/master/doc/screencast-3.gif)
- [4-gif](https://raw.github.com/abo-abo/lispy/master/doc/oneline-multiline.gif)

Synopsis for
[5](https://raw.github.com/abo-abo/lispy/master/doc/screencast-4.gif):
find a function and make it echo its doc-string.

1. Use `lispy-goto` "g" to find the function.
2. Use `lispy-ace-symbol` "a" to mark its doc-string.
3. Use `lispy-clone` "c" to clone region. Note that the region doesn't disappear.
4. Use `lispy-parens` "(" to wrap region.
5. Insert "message " and use `lispy-forward` "]" to exit the list and move into special.
6. Use `lispy-mark-list` "m" to mark current list so that we can move it later.
7. Use `lispy-move-down` "s" to move marked sexp all the way down.
8. Use `lispy-forward` "]" twice to exit defun.
9. Use `lispy-eval` "e" to eval our changes.
10. Use `lispy-undo` "u" to see that the change is now in effect.
