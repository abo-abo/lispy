;;* Welcome to the `lispy-mode' Tutor
;;
;; `lispy-mode' is a very powerful way to edit LISP code that has many
;; commands, too many to explain in a tutor such as this. This tutor
;; is designed to describe enough of the commands that you will be
;; able to easily use `lispy-mode' as an all-purpose LISP editor.
;;
;; The approximate time required to complete the tutor is 25-30
;; minutes, depending upon how much time is spent with
;; experimentation.
;;
;; ATTENTION:
;; The commands in the lessons will modify the code. You can always
;; revert the file using git.
;;
;; It is important to remember that this tutor is set up to teach by
;; use.  That means that you need to execute the commands to learn
;; them properly.  If you only read the text, you will forget the
;; commands!
;;
;; Remember that this is Emacs, you can rebind any binding that you
;; don't like. However, a lot of thought has been put into the current
;; bindings so don't be surprised if you find stuff inconvenient after
;; rebinding without thinking it through.
;;
;; Your point should now be at beginning of the first line.
;; Press =M-<= if it isn't.
;;
;; Press =J= to move to the next outline downwards.
;;
;; You should now be at Lesson 1.1.
;;
;; Press =i= to show the lesson.
;; Press =N= to narrow to the lesson.
;;
;;** Lesson 1.1: TUTORIAL NAVIGATION
;; This tutorial uses lispy outlines for navigation.
;; 
;; Outlines provide a way of navigating code in a tree-like fashion.
;; The outlines themselves form the nodes of the tree.
;; 
;; An outline begins with a comment ";;" followed by or more asterisks.
;; The number of asterisks determines the outline's depth in the tree.
;; Your cursor should be at the start of the outline:
;; ";;** Lesson 1.1: TUTORIAL NAVIGATION"
;; 
;; Place it there if it isn't already.
;;
;; Lispy enables special keybindings when your cursor is placed on an
;; object lispy recognizes.
;; 
;; Here's a few to get started:
;; =J= moves to the next outline
;; =K= moves to the previous outline
;; =i= cycles the expansion of the outline and its children.
;;
;; Press =J= and =K= a few times to navigate the outlines in this lesson.
;; Try cycling the expansion of outlines with =i=.
;; 
;; When you're finished proceed to Exercise 1.
;; 
;;*** Exercise 1
;; Lispy can "narrow" or "widen" outlines.
;; 
;; Narrowing an outline hides all the outlines in the buffer except
;; for the outline at the cursor.  This is useful when you want to
;; view or operate on the contents of a single outline.
;;
;; Press =N= to narrow to the outline for Exercise 1.
;;
;; Widening shows all outlines that have been hidden through
;; narrowing.
;;
;; Press =W= to widen the outlines. Notice how the rest of the
;; tutorial is displayed.
;; 
;; Move to Exercise 2, show it, and narrow to it using the
;; shortcuts you've learned.
;;
;;*** Exercise 2
;; Let's wrap up this lesson with one final shortcut.
;; 
;; =2-I= will show the condensed outline structure for all the visible
;; outlines in the buffer.
;; 
;; Press =W= to show the hidden outlines.
;; Press =2-I= and proceed to Lesson 1.2
;; 
;;** Lesson 1.2: MOVING THE CURSOR
;;
;; You can undo the narrow with `widen' command bound to =C-x nw=.  You
;; don't need to do this now, as this lesson is designed with narrowing in
;; place.
;;
;; Def. 1: The current cursor position, referred to further as point,
;; is called special when either of the conditions holds true:
;;   - the point is before ( or [ or {, referred to further as left paren
;;   - the point is after ) or ] or }, referred to further as right paren
;;   - the point is at the start of a comment
;;   - the region is active
;;
;; When the point is special, lower and upper case letters will call
;; commands instead of self-inserting (these commands can depend on
;; the particular variation of special as described above), and the
;; digit keys will call `digit-argument'.
;;
;; ** To test =f=, bound to `lispy-flow' **
;;
;; 1. Move into the special position: the first char of the line
;; indicated below.
;; 2. Press =f= to move the cursor to the next paren in current direction.
;; <--- special
;;
;; Hold =f= to move repeatedly until you reach "Skip a bit, Brother..."
(with-output-to-string
  (defvar weapon "hand granade")
  (let ((name "Saint Atilla"))
    (princ (format "And %s raised the %s up on high, saying, " name weapon)))
  (defun cite (str)
    (format "\"%s\"" str))
  (princ (cite (concat
                "O Lord, bless this thy "
                weapon
                ", that with it thou mayst blow thine "
                "enemies to tiny bits, in thy mercy.")))
  (princ " And the Lord did grin. ")
  (princ "And the people did feast upon ")
  (princ (mapconcat (lambda (x) (format "the %ss" x))
                    '("lamb" "sloth" "carp" "anchovie" "orangutan"
                      "breakfast cereal" "fruit bat")
                    ", and "))
  (princ ", and large chu...")
  (princ "\n\nSkip a bit, Brother..."))
;; 1. Now press =d= bound to `lispy-different' to switch to the different
;; side of the list. Press it a few times to get the feeling.
;;
;; 2. Making sure that you are at the right paren, press and hold =f= until
;; you reach "hand granade".
;;
;; 3. Press =d= and hold =f=, making circles around the expressions until
;; you're comfortable.
;;
;; ** To move the cursor, press the =h=, =j=, =k=, =l= keys as indicated. **
;;
;;       ^
;;       k               Hint:  The =h= key is at the left and moves left.
;; < h       l >                The =l= key is at the right and moves right.
;;       j                      The =j= key looks like a down arrow.
;;       v
;;
;; Note that if it's impossible to move, the commands will fail silently.
;; In common code situations, if =j= doesn't work, it means that you've
;; reached the last expression of the parent list. The typical follow-up is
;; on =h= followed by either =j= or =k=.
;;
;; Use your knowledge of =f= and =d= to setup the point in various places
;; and see what the arrow keys do there. But it's actually very simple: =j=
;; and =k= have a guarantee not to leave the parent list.
;;
;; <--- To end the lesson, move the point here and press =W= (`widen').
;;** Lesson 1.3: EXITING AND ENTERING SPECIAL
;;
;; This is actually very straightforward: just press =C-f= or =C-n= or
;; =C-p= or anything else that makes the point not special.
;;
;; The bindings that only work in special are futher referred to as
;; local. All the rest are global - they work everywhere, regardless
;; of the point. Here are the global bindings that are particularly
;; useful for entering special.
;;
;; - =[= calls `lispy-backward': when not in special, move to the left
;;   paren of the containing list. Otherwise, behave similar to
;;   `backward-list'.
;;
;; - =]= calls `lispy-forward': when not in special, move to the right
;;   paren of the containing list. Otherwise, behavir similar to
;;   `forward-list'.
;;
;; - =M-m= calls `lispy-mark-symbol': mark the current symbol with the
;;   region. When the region is already active, try to extend it by
;;   one symbol.
;;
;;** Lesson 1.4: LISP EDITING - DELETION
;;
;; To delete things, first you need to have a lot of them.
;;
;; 1. You can generate a lot of code by moving the point before
;; (with-output-to-string and pressing =c= bound to `lispy-clone' a
;; few times.
;;
;; 2. Now, press =C-d= bound to `lispy-delete' to delete the whole
;; sexp. Do this until you have only two top-level sexps left.
;;
;; 3. Press =f= to move inside the top-level sexp and hold =C-d= until
;; everything inside is deleted. Note how the top-level sexp is
;; deleted after all its children are gone.
;;
;; 4. Make another clone with =c= and switch to the different side
;; with =d=.  Do a single =f= and practice pressing =DEL= bound to
;; `lispy-delete-backward'.
;;
;; 5. Use conventional means to position the point at either side of
;; the string and the quote and test what both =C-d= and =DEL= do.
(with-output-to-string
  (defvar weapon "hand granade")
  (let ((name "Saint Atilla"))
    (princ (format "And %s raised the %s up on high, saying, " name weapon)))
  (defun cite (str)
    (format "\"%s\"" str))
  (princ (cite (concat
                "O Lord, bless this thy "
                weapon
                ", that with it thou mayst blow thine "
                "enemies to tiny bits, in thy mercy.")))
  (princ " And the Lord did grin. ")
  (princ "And the people did feast upon ")
  (princ (mapconcat (lambda (x) (format "the %ss" x))
                    '("lamb" "sloth" "carp" "anchovie" "orangutan"
                      "breakfast cereal" "fruit bat")
                    ", and "))
  (princ ", and large chu...")
  (princ "\n\nSkip a bit, Brother..."))
;;** Lesson 1.5: LISP EDITING - INSERTION
;;
;; Basic insertion bindings:
;;
;; =(= - inserts () and goes backward one char in code, self-inserts
;; in comments and strings.
;; ={= - inserts {} and goes backward one char everywhere
;; =}= - inserts [] and goes backward one char everywhere
;; ="= - inserts "" and goes backward one char, auto-quotes in strings
;;
;; All four of the pairs above will try to add once space where
;; appropriate, so to insert "(list ())", press =(list(=.
;;
;; If you want any of the mentined keys to self-insert once, remember
;; that prefixing any key with =C-q= will do that.
;;
;; All four pairs will wrap the current thing when the region is active.
;; All four pairs will wrap the current symbol when prefixed with =C-u=.
;;
;;** Lesson 1.6: LISP EDITING - APPENDING
;;
;; You can append the current list:
;;
;; - from the front with =2 SPC=
;; - from the back with =3 SPC=
;; - from the back with a newline =4 SPC=
;;** Lesson 1.7: OUTLINE NAVIGATION
;;
;; Here, several bindings depend on conditions additional to being in
;; special.  Some of them are a superset of others, for example
;; COMMENT is a superset of OUTLINE, since all outlines are comments.
;;
;; When at COMMENT, you can:
;;
;; - navigate to the next outline with =j=,
;; - navigate to the previous outline with =k=.
;; - navigate to the first code paren of the outline with =f=.
;;
;; When at OUTLINE, you can:
;;
;; - fold and unfold the outline with =i=,
;; - promote the outline with =h=,
;; - demote the outline with =l=,
;; - recenter / undo recenter with =v=,
;; - create a new outline with =a=,
;; - move to the end of the oneline heading with =t=,
;; - narrow to the outline with =N=,
;; - widen with =W=.
;;
;; Anywhere, you can:
;;
;; - navigate to the next outline with =J=,
;; - navigate to the previous outline with =K=,
;; - toggle folding to all outlines of level 1 with =I=,
;; - get the table of contents with a prefix argument and =I=, e.g. =2I=,
;;
;; Also note that =I= has a global alias =S-TAB=, which you can use
;; even when not is special. It mirrors the popular `org-mode'
;; package, with the difference that it's a two-way toggle instead of
;; three-way. You can get the third option with a prefix argument.
;;** Lesson 1.8: MOVING THE REGION
