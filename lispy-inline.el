;;; lispy-inline.el --- inline arglist and documentation.

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lispy
;; Version: 0.3
;; Keywords: lisp

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display current function arguments or docstring in an in-place
;; overlay.

;;; Code:

(defface lispy-face-hint
  '((t (:background "#fff3bc" :foreground "black")))
  "Basic hint face."
  :group 'lispy)

(defvar lispy-overlay nil
  "Hint overlay instance.")

(defvar lispy-hint-pos nil
  "Point position where the hint should be (re-) displayed.")

;; ——— Commands ————————————————————————————————————————————————————————————————
(defun lispy-arglist-inline ()
  "Display arglist for `lispy--current-function'."
  (interactive)
  (let ((deleted))
    (when (overlayp lispy-overlay)
      (delete-overlay lispy-overlay)
      (setq lispy-overlay nil)
      (setq deleted t))
    (save-excursion
      (unless (looking-at "(")
        (lispy-out-backward 1))
      (when (or (not deleted) (not (= lispy-hint-pos (point))))
        (let ((sym (lispy--current-function)))
          (cond ((fboundp sym)
                 (setq lispy-hint-pos (point))
                 (lispy--show (lispy--arglist sym)))))))))

(defun lispy-describe-inline ()
  "Display `documentation' inline."
  (interactive)
  (let ((deleted))
    (when (overlayp lispy-overlay)
      (delete-overlay lispy-overlay)
      (setq lispy-overlay nil)
      (setq deleted t))
    (save-excursion
      (unless (looking-at "(")
        (lispy-out-backward 1))
      (when (or (not deleted) (not (= lispy-hint-pos (point))))
        (let ((sym (lispy--current-function)))
          (cond ((fboundp sym)
                 (setq lispy-hint-pos (point))
                 (lispy--show (documentation sym)))))))))

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun lispy--arglist (symbol)
  "Get arglist for SYMBOL."
  (let (doc)
    (if (setq doc (help-split-fundoc (documentation symbol t) symbol))
        (car doc)
      (prin1-to-string
       (cons symbol (help-function-arglist symbol))))))

(defun lispy--pad-string (str width)
  "Pad each line of STR with WIDTH spaces."
  (let ((padding (make-string width ?\ )))
    (mapconcat (lambda(x) (concat padding x))
               (split-string str "\n")
               "\n")))

(defun lispy--show (str)
  "Show STR hint."
  (setq str (lispy--pad-string
             (propertize str 'face 'lispy-face-hint)
             (string-width (buffer-substring
                            (line-beginning-position)
                            (point)))))
  (save-excursion
    (goto-char lispy-hint-pos)
    (forward-line -1)
    (end-of-line)
    (setq str (concat "\n" str (make-string 1 (char-after))))
    (font-lock-unfontify-region (point) (+ (point) 1))
    (if lispy-overlay
        (progn
          (move-overlay lispy-overlay (point) (+ (point) 1))
          (overlay-put lispy-overlay 'invisible nil))
      (setq lispy-overlay (make-overlay (point) (+ (point) 1)))
      (overlay-put lispy-overlay 'priority 9999))
    (overlay-put lispy-overlay 'display str)
    (overlay-put lispy-overlay 'after-string "")))

(provide 'lispy-inline)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; lispy-inline.el ends here
