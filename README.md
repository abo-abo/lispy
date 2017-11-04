[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/abo-abo/lispy.svg?branch=master)](https://travis-ci.org/abo-abo/lispy)
[![Coverage Status](https://coveralls.io/repos/abo-abo/lispy/badge.svg?branch=master)](https://coveralls.io/r/abo-abo/lispy?branch=master)
[![MELPA](http://melpa.org/packages/lispy-badge.svg)](http://melpa.org/#/lispy)
[![MELPA Stable](http://stable.melpa.org/packages/lispy-badge.svg)](http://stable.melpa.org/#/lispy)

<p align="center">
<img src="https://raw.githubusercontent.com/abo-abo/lispy/master/images/lispy-logo.png"
   alt="lispy logo"/>
</p>

> short and sweet LISP editing

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
    - [Relation to vi](#relation-to-vi)
    - [Features](#features)
    - [Function reference](#function-reference)
- [Getting Started](#getting-started)
    - [Installation instructions](#installation-instructions)
        - [via MELPA](#via-melpa)
        - [via el-get](#via-el-get)
    - [Configuration instructions](#configuration-instructions)
    - [Customization instructions](#customization-instructions)
- [Operating on lists](#operating-on-lists)
    - [How to get into list-editing mode (special)](#how-to-get-into-list-editing-mode-special)
    - [Digit keys in special](#digit-keys-in-special)
    - [How to get out of special](#how-to-get-out-of-special)
    - [List commands overview](#list-commands-overview)
        - [Inserting pairs](#inserting-pairs)
        - [Reversible commands](#reversible-commands)
        - [Keys that modify whitespace](#keys-that-modify-whitespace)
        - [Command chaining](#command-chaining)
        - [Navigating with `avy`-related commands](#navigating-with-ace-jump-mode-related-commands)
- [Operating on regions](#operating-on-regions)
    - [Ways to activate region](#ways-to-activate-region)
    - [Move region around](#move-region-around)
    - [Switch to the other side of the region](#switch-to-the-other-side-of-the-region)
    - [Grow/shrink region](#growshrink-region)
    - [Commands that operate on region](#commands-that-operate-on-region)
- [IDE-like features](#ide-like-features)
- [Demos](#demos)
    - [[Demo 1: Practice generating code](http://abo-abo.github.io/lispy/demo-1)](#demo-1-practice-generating-codehttpabo-abogithubiolispydemo-1)
    - [[Demo 2: The substitution model for procedure application](http://abo-abo.github.io/lispy/demo-2)](#demo-2-the-substitution-model-for-procedure-applicationhttpabo-abogithubiolispydemo-2)
    - [[Demo 3: Down the rabbit hole](http://abo-abo.github.io/lispy/demo-3)](#demo-3-down-the-rabbit-holehttpabo-abogithubiolispydemo-3)
    - [[Demo 4: Project Euler p100 and Clojure](http://abo-abo.github.io/lispy/demo-4)](#demo-4-project-euler-p100-and-clojurehttpabo-abogithubiolispydemo-4)
    - [[Demo 5: ->>ification](http://abo-abo.github.io/lispy/demo-5)](#demo-5--ificationhttpabo-abogithubiolispydemo-5)
    - [[Demo 6: cond->if->cond](http://abo-abo.github.io/lispy/demo-6)](#demo-6-cond-if-condhttpabo-abogithubiolispydemo-6)
- [Screencasts](#screencasts)

<!-- markdown-toc end -->

# Introduction

This package reimagines Paredit - a popular method to navigate and
edit LISP code in Emacs.

The killer-feature are the short bindings:

|       command                |   binding        |  binding     | command
|:-----------------------------|:----------------:|:------------:|:------------------
|`paredit-forward`             | <kbd>C-M-f</kbd> | <kbd>j</kbd> | `lispy-down`
|`paredit-backward`            | <kbd>C-M-b</kbd> | <kbd>k</kbd> | `lispy-up`
|`paredit-backward-up`         | <kbd>C-M-u</kbd> | <kbd>h</kbd> | `lispy-left`
|`paredit-forward-up`          | <kbd>C-M-n</kbd> | <kbd>l</kbd> | `lispy-right`
|`paredit-raise-sexp`          | <kbd>M-r</kbd>   | <kbd>r</kbd> | `lispy-raise`
|`paredit-convolute-sexp`      | <kbd>M-?</kbd>   | <kbd>C</kbd> | `lispy-convolute`
|`paredit-forward-slurp-sexp`  | <kbd>C-)</kbd>   | <kbd>></kbd> | `lispy-slurp`
|`paredit-forward-barf-sexp`   | <kbd>C-}</kbd>   | <kbd><</kbd> | `lispy-barf`
|`paredit-backward-slurp-sexp` | <kbd>C-(</kbd>   | <kbd>></kbd> | `lispy-slurp`
|`paredit-backward-barf-sexp`  | <kbd>C-{</kbd>   | <kbd><</kbd> | `lispy-barf`

Most of more than 100 interactive commands that `lispy` provides are
bound to <kbd>a</kbd>-<kbd>z</kbd> and <kbd>A</kbd>-<kbd>Z</kbd> in
`lispy-mode`.  You can see the full command reference with many
examples [here](http://abo-abo.github.io/lispy/).

The price for these short bindings is that they are only active when:

- the point is before an open paren: `(`, `[` or `{`
- the point is after a close paren: `)`, `]` or `}`
- the region is active

The advantage of short bindings is that you are more likely to use
them.  As you use them more, you learn how to combine them, increasing
your editing efficiency.

To further facilitate building complex commands from smaller commands,
`lispy-mode` binds `digit-argument` to <kbd>0</kbd>-<kbd>9</kbd>.  For
example, you can mark the third element of the list with
<kbd>3m</kbd>.  You can then mark third through fifth element (three
total) with <kbd>2></kbd> or <kbd>>></kbd>. You can then move the
selection to the last three elements of the list with <kbd>99j</kbd>.

If you are currently using Paredit, note that `lispy-mode` and
`paredit-mode` can actually coexist with very few conflicts, although
there would be some redundancy.

## Relation to vi

The key binding method is influenced by vi, although this isn't modal
editing *per se*.

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

|                  | insert "j"     | forward-list
|------------------|:--------------:|:-------------:
|Emacs             | <kbd>j</kbd>   | <kbd>C-M-n</kbd>
|vi in insert mode | <kbd>j</kbd>   | impossible
|vi in normal mode | impossible     | <kbd>j</kbd>
|lispy             | <kbd>j</kbd>   | <kbd>j</kbd>

Advantages/disadvantages:

- Emacs can both insert and call commands without switching modes (since it has none),
  but the command bindings are long
- vi has short command bindings, but you have to switch modes between inserting and calling commands
- lispy has short command bindings and doesn't need to switch modes

Of course it's not magic, lispy needs to have normal/insert mode to
perform both functions with <kbd>j</kbd>.  The difference from vi is
that the mode is **explicit** instead of **implicit** - it's
determined by the point position or the region state:

- you are in normal mode when the point is before/after paren or the
  region is active
- otherwise you are in insert mode

So people who generally like Emacs bindings (like me) can have the
cake and eat it too (no dedicated insert mode + shorter key bindings).
While people who like vi can still get an experience that's reasonably
close to vi for LISP editing (since vi's line-based approach isn't
very appropriate for LISP anyway).

But if you ask:

> What if I want to insert when the point is before/after paren or the region is active?

The answer is that because of the LISP syntax you don't want to write
this:

```cl
j(progn
   (forward-char 1))k
```

Also, Emacs does nothing special by default when the region is active
and you press a normal key, so new commands can be called in that
situation.

## Features

- Basic navigation by-list and by-region:
    - <kbd>h</kbd> moves left
    - <kbd>j</kbd> moves down
    - <kbd>k</kbd> moves up
    - <kbd>l</kbd> moves right
    - <kbd>f</kbd> steps inside the list
    - <kbd>b</kbd> moves back in history for all above commands

- Paredit transformations, callable by plain letters:
    - <kbd>></kbd> slurps
    - <kbd><</kbd> barfs
    - <kbd>r</kbd> raises
    - <kbd>C</kbd> convolutes
    - <kbd>s</kbd> moves down
    - <kbd>w</kbd> moves up
- IDE-like features for Elisp, Clojure, Scheme, Common Lisp, Hy, Python and Julia:
    - <kbd>e</kbd> evals
    - <kbd>E</kbd> evals and inserts
    - <kbd>g</kbd> jumps to any tag in the current directory with semantic
    - <kbd>G</kbd> jumps to any tag in the current file
    - <kbd>M-.</kbd> jumps to symbol, <kbd>M-,</kbd> jumps back
    - <kbd>F</kbd> jumps to symbol, <kbd>D</kbd> jumps back
    - <kbd>C-1</kbd> shows documentation in an overlay
    - <kbd>C-2</kbd> shows arguments in an overlay
    - [<kbd>Z</kbd>](http://abo-abo.github.io/lispy/#lispy-edebug-stop) breaks
      out of `edebug`, while storing current function's arguments

Some pictures [here](#ide-like-features).
- Code manipulation:
    - <kbd>i</kbd> prettifies code (remove extra space, hanging parens ...)
    - <kbd>xi</kbd> transforms `cond` expression to equivalent `if` expressions
    - <kbd>xc</kbd> transforms `if` expressions to an equivalent `cond` expression
    - <kbd>xf</kbd> flattens function or macro call (extract body and substitute arguments)
    - <kbd>xr</kbd> evals and replaces
    - <kbd>xl</kbd> turns current `defun` into a `lambda`
    - <kbd>xd</kbd> turns current `lambda` into a `defun`
    - <kbd>O</kbd> formats the code into one line
    - <kbd>M</kbd> formats the code into multiple lines
- Misc. bindings:
    - outlines navigation/folding (<kbd>J</kbd>, <kbd>K</kbd>, <kbd>I</kbd>, <kbd>i</kbd>)
    - narrow/widen (<kbd>N</kbd>, <kbd>W</kbd>)
    - `ediff` (<kbd>b</kbd>, <kbd>B</kbd>)
    - `ert` (<kbd>T</kbd>)
    - `edebug` (<kbd>xe</kbd>)

## Function reference
Most functions are cataloged and described at http://abo-abo.github.io/lispy/.

# Getting Started
## Installation instructions
### via MELPA

It's easiest/recommended to install from [MELPA](http://melpa.org/).
Here's a minimal MELPA configuration for your `~/.emacs`:

```cl
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
```

Afterwards, <kbd>M-x package-install RET lispy RET</kbd> (you might
want to <kbd>M-x package-refresh-contents RET</kbd> beforehand if
you haven't done so recently).

### via el-get

[el-get](https://github.com/dimitri/el-get) also features a lispy recipe.
Use <kbd>M-x el-get-install RET lispy RET</kbd> to install.

## Configuration instructions
**Enable lispy automatically for certain modes**

After installing, you can call <kbd>M-x lispy-mode</kbd> for any
buffer with a LISP dialect source.  To have `lispy-mode` activated
automatically, use something like this:


```cl
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
```

**Enable lispy for `eval-expression`**

Although I prefer to eval things in `*scratch*`, sometimes
<kbd>M-:</kbd> - `eval-expression` is handy.  Here's how to use lispy
in the minibuffer during `eval-expression`:

```cl
(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
```

## Customization instructions

If you want to replace some of the `lispy-mode`'s bindings you can do
it like this:

```cl
(eval-after-load "lispy"
  `(progn
     ;; replace a global binding with own function
     (define-key lispy-mode-map (kbd "C-e") 'my-custom-eol)
     ;; replace a global binding with major-mode's default
     (define-key lispy-mode-map (kbd "C-j") nil)
     ;; replace a local binding
     (lispy-define-key lispy-mode-map "s" 'lispy-down)))
```

# Operating on lists

## How to get into list-editing mode (special)

The plain keys will call commands when:
- the point is positioned before paren
- the point is positioned after paren
- the region is active

When one of the first two conditions is true, I say that the point is
special. When the point is special, it's very clear to which sexp the
list-manipulating command will be applied to, what the result be and
where the point should end up afterwards.  You can enhance this effect
with `show-paren-mode` or similar.

Here's an illustration to this effect, with `lispy-clone` (here, `|`
represents the point):

|before              | key          | after
|:-------------------|:------------:|:-----------------------
|`(looking-at "(")\|` | <kbd>c</kbd> |  `(looking-at "(")`
|                    |              |  `(looking-at "(")\|`

|before              | key          | after
|:-------------------|:------------:|:-----------------------
|`\|(looking-at "(")` | <kbd>c</kbd> |  `\|(looking-at "(")`
|                    |              |   ` (looking-at "(")`

You can use plain Emacs navigation commands to get into special, or you can use
some of the dedicated commands:

Key Binding     | Description
----------------|-----------------------------------------------------------
<kbd>]</kbd>    | `lispy-forward` - move to the end of the closest list, analogous to <kbd>C-M-n</kbd> (`forward-list`)
<kbd>&#91;</kbd>| `lispy-backward` - move to the start of the closest list, analogous to <kbd>C-M-p</kbd> (`backward-list`)
<kbd>C-3</kbd>  | `lispy-right` - exit current list forwards, analogous to `up-list`
<kbd>)</kbd>    | `lispy-right-nostring` exit current list forwards, but self-insert in strings and comments

These are the few lispy commands that don't care whether the point is
special or not. Other such bindings are <kbd>DEL</kbd>, <kbd>C-d</kbd>, <kbd>C-k</kbd>.

Special is useful for manipulating/navigating lists.  If you want to
manipulate symbols, use [region selection](#operating-on-regions)
instead.

## Digit keys in special

When special, the digit keys call `digit-argument` which is very
useful since most lispy commands accept a numeric argument.
For instance, <kbd>3c</kbd> is equivalent to <kbd>ccc</kbd> (clone sexp 3 times), and
<kbd>4j</kbd> is equivalent to <kbd>jjjj</kbd> (move point 4 sexps down).

Some useful applications are <kbd>9l</kbd> and <kbd>9h</kbd> - they exit list forwards
and backwards respectively at most 9 times which makes them
effectively equivalent to `end-of-defun` and `beginning-of-defun`.  Or
you can move to the last sexp of the file with <kbd>999j</kbd>.

## How to get out of special

To get out of the special position, you can use any of the good-old
navigational commands such as <kbd>C-f</kbd> or <kbd>C-n</kbd>.
Additionally <kbd>SPC</kbd> will break out of special to get around the
situation when you have the point between the open parens like this

    (|(

and want to start inserting; <kbd>SPC</kbd> will change the code to
this:

    (| (

## List commands overview
### Inserting pairs

Here's a list of commands for inserting [pairs](http://abo-abo.github.io/lispy/#lispy-pair):

key               | command
------------------|-------------------------------------------------------------------
  <kbd>(</kbd>    | [`lispy-parens`](http://abo-abo.github.io/lispy/#lispy-parens)
  <kbd>{</kbd>    | [`lispy-braces`](http://abo-abo.github.io/lispy/#lispy-braces)
  <kbd>}</kbd>    | [`lispy-brackets`](http://abo-abo.github.io/lispy/#lispy-brackets)
  <kbd>"</kbd>    | [`lispy-quotes`](http://abo-abo.github.io/lispy/#lispy-quotes)

### Reversible commands

A lot of Lispy commands come in pairs - one reverses the other:

 key            | command                  | key                              | command
----------------|--------------------------|----------------------------------|----------------------
 <kbd>j</kbd>   | `lispy-down`             | <kbd>k</kbd>                     | `lispy-up`
 <kbd>s</kbd>   | `lispy-move-down`        | <kbd>w</kbd>                     | `lispy-move-up`
 <kbd>></kbd>   | `lispy-slurp`            | <kbd><</kbd>                     | `lispy-barf`
 <kbd>c</kbd>   | `lispy-clone`            | <kbd>C-d</kbd> or <kbd>DEL</kbd> |
 <kbd>C</kbd>   | `lispy-convolute`        | <kbd>C</kbd>                     | reverses itself
 <kbd>d</kbd>   | `lispy-different`        | <kbd>d</kbd>                     | reverses itself
 <kbd>M-j</kbd> | `lispy-split`            | <kbd>+</kbd>                     | `lispy-join`
 <kbd>O</kbd>   | `lispy-oneline`          | <kbd>M</kbd>                     | `lispy-multiline`
 <kbd>S</kbd>   | `lispy-stringify`        | <kbd>C-u "</kbd>                 | `lispy-quotes`
 <kbd>;</kbd>   | `lispy-comment`          | <kbd>C-u ;</kbd>                 | `lispy-comment`
 <kbd>xi</kbd>  | `lispy-to-ifs`           | <kbd>xc</kbd>                    | `lispy-to-cond`

### Keys that modify whitespace

These commands handle whitespace in addition to inserting the expected
thing.

 key            | command
----------------|---------------------------
 <kbd>SPC</kbd> | `lispy-space`
 <kbd>:</kbd>   | `lispy-colon`
 <kbd>^</kbd>   | `lispy-hat`
 <kbd>C-m</kbd> | `lispy-newline-and-indent`

### Command chaining

Most special commands will leave the point special after they're
done.  This allows to chain them as well as apply them
continuously by holding the key.  Some useful hold-able keys are
<kbd>jkf<>cws;</kbd>.
Not so useful, but fun is <kbd>/</kbd>: start it from `|(` position and hold
until all your Lisp code is turned into Python :).

### Navigating with `avy`-related commands

 key            | command
----------------|--------------------------
 <kbd>q</kbd>   | `lispy-ace-paren`
 <kbd>Q</kbd>   | `lispy-ace-char`
 <kbd>a</kbd>   | `lispy-ace-symbol`
 <kbd>H</kbd>   | `lispy-ace-symbol-replace`
 <kbd>-</kbd>   | `lispy-ace-subword`

<kbd>q</kbd> - `lispy-ace-paren` jumps to a "(" character within current
top-level form (e.g. `defun`). It's much faster than typing in the
`avy` binding + selecting "(", and there's less candidates,
since they're limited to the current top-level form.

<kbd>a</kbd> - `lispy-ace-symbol` will let you select which symbol to
mark within current form. This can be followed up with e.g. eval,
describe, follow, raise etc. Or you can simply <kbd>m</kbd> to
deactivate the mark and edit from there.

<kbd>-</kbd> - `lispy-ace-subword` is a niche command for a neat combo. Start with:

    (buffer-substring-no-properties
     (region-beginning)|)

Type <kbd>c</kbd>, <kbd>-</kbd>, <kbd>b</kbd> and <kbd>C-d</kbd> to get:

    (buffer-substring-no-properties
     (region-beginning)
     (region-|))

Fill `end` to finish the statement.

# Operating on regions
Sometimes the expression that you want to operate on isn't bounded by parens.
In that case you can mark it with a region and operate on that.

## Ways to activate region
While in special:
- Mark a sexp with <kbd>m</kbd> - `lispy-mark-list`
- Mark a symbol within sexp <kbd>a</kbd> - `lispy-ace-symbol`.

While not in special:
- <kbd>C-SPC</kbd> - `set-mark-command`
- mark a symbol at point with <kbd>M-m</kbd> - `lispy-mark-symbol`
- mark containing expression (list or string or comment) with <kbd>C-M-,</kbd> - `lispy-mark`

## Move region around

The arrow keys <kbd>j</kbd>/<kbd>k</kbd> will move the region up/down within the current
list.  The actual code will not be changed.

## Switch to the other side of the region

Use <kbd>d</kbd> - `lispy-different` to switch between different sides
of the region. The side is important since the grow/shrink operations
apply to current side of the region.

## Grow/shrink region

Use a combination of:
- <kbd>></kbd> - `lispy-slurp` - extend by one sexp from the current side. Use digit
  argument to extend by several sexps.
- <kbd><</kbd> - `lispy-barf` - shrink by one sexp from the current side. Use digit
  argument to shrink by several sexps.

The other two arrow keys will mark the parent list of the current region:

- <kbd>h</kbd> - `lispy-left` - mark the parent list with the point on the left
- <kbd>l</kbd> - `lispy-right` - mark the parent list with the point on the right

To do the reverse of the previous operation, i.e. to mark the first
child of marked list, use <kbd>i</kbd> - `lispy-tab`.

## Commands that operate on region
- <kbd>m</kbd> - `lispy-mark-list` - deactivate region
- <kbd>c</kbd> - `lispy-clone` - clone region and keep it active
- <kbd>s</kbd> - `lispy-move-down` - move region one sexp down
- <kbd>w</kbd> - `lispy-move-up` - move region one sexp up
- <kbd>u</kbd> - `lispy-undo` - deactivate region and undo
- <kbd>t</kbd> - `lispy-teleport` - move region inside the sexp you select with `lispy-ace-paren`
- <kbd>C</kbd> - `lispy-convolute` - exchange the order of application of two sexps that contain region
- <kbd>n</kbd> - `lispy-new-copy` - copy region as kill without deactivating the mark
- <kbd>P</kbd> - `lispy-paste` - replace region with current kill

# IDE-like features

These features are specific to the Lisp dialect used.  Currently Elisp
and Clojure (via `cider`) are supported.  There's also basic
evaluation support for:

- Scheme (via `geiser`)
- Common lisp (via `slime` or `sly`).
- Hy (via `comint`).
- Python (via `comint` and `jedi`).
- Julia (via `julia-shell`).

**`lispy-describe-inline`**

Bound to <kbd>C-1</kbd>. Show the doc for the current function inline.

<kbd>C-h f</kbd> is fine, but the extra buffer, and having to navigate to a symbol
is tiresome. <kbd>C-1</kbd> toggles on/off the inline doc for current function.
No extra buffer necessary:

![screenshot](https://raw.github.com/abo-abo/lispy/master/images/doc-elisp.png)

Here's how it looks for Clojure:

![screenshot](https://raw.github.com/abo-abo/lispy/master/images/doc-clojure.png)

**`lispy-arglist-inline`**

Bound to <kbd>C-2</kbd>. Show arguments for current function inline.

`eldoc-mode` is cool, but it shows you arguments *over there* and
you're writing *over here*!. No problem, <kbd>C-2</kbd> fixes that:

![screenshot](https://raw.github.com/abo-abo/lispy/master/images/arglist-elisp.png)

As you see, normal, &optional and &rest arguments have each a
different face. Here's how it looks for Clojure:

![screenshot](https://raw.github.com/abo-abo/lispy/master/images/arglist-clojure.png)

**`lispy-goto`**

Bound to <kbd>g</kbd>.

Use completion to select a symbol to jump to from all top-level symbols in the in current directory.

Works out of the box for Elisp, Scheme and Common Lisp.
[clojure-semantic](https://github.com/kototama/clojure-semantic) is
required for Clojure.

**`lispy-eval`**

There's a feature similar to `ipython-notebook`. Evaluating an Emacs
outline will evaluate all of the outline's code and echo the result of
the last expression. When an outline ends with a colon (`:`), the
result will instead be inserted into the buffer. If the evaluation
result changes for whatever reason, it will be replaced after each
subsequent <kbd>e</kbd>.

Python and Julia currently have a slightly better notebook support,
pressing <kbd>e</kbd> on the parent outline will evaluate all the
children outlines sequentially. This allows to arrange scripts
hierarchically, with relatively few top-level outlines and relatively
many total outlines. Each outline's output can be examined by adding a
`:` to the title of the outline.

The following example shows a buffer before and after pressing <kbd>e</kbd>.

![lispy-python-notebook.png](https://raw.githubusercontent.com/wiki/abo-abo/lispy/images/lispy-python-notebook.png)

There is one top-level outline, with one level-2 child, which in turn
has a four level-3 children. Three of these children end in `:`, so
their output will be updated after the eval.

# Demos

## [Demo 1: Practice generating code](http://abo-abo.github.io/lispy/demo-1)
## [Demo 2: The substitution model for procedure application](http://abo-abo.github.io/lispy/demo-2)
## [Demo 3: Down the rabbit hole](http://abo-abo.github.io/lispy/demo-3)
## [Demo 4: Project Euler p100 and Clojure](http://abo-abo.github.io/lispy/demo-4)
## [Demo 5: ->>ification](http://abo-abo.github.io/lispy/demo-5)
## [Demo 6: cond->if->cond](http://abo-abo.github.io/lispy/demo-6)

# Screencasts

- The older stuff can be found on [vimeo](http://vimeo.com/user24828177/videos).
- The newer stuff is on https://www.youtube.com/user/abo5abo/videos.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
