;;; le-python.el --- lispy support for Python. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel

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

;;; Code:

(require 'python)
(require 'json)

(defun lispy-eval-python ()
  (let (str bnd res)
    (setq str
          (save-excursion
            (cond ((region-active-p)
                   (setq str (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
                   (if (= (cl-count ?\n str) 0)
                       str
                     ;; get rid of "unexpected indent"
                     (replace-regexp-in-string
                      (concat
                       "^"
                       (save-excursion
                         (goto-char (region-beginning))
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (point))))
                      "" (lispy--string-dwim))))
                  ((looking-at lispy-outline)
                   (string-trim-right
                    (lispy--string-dwim
                     (lispy--bounds-dwim))))
                  ((save-excursion
                     (when (looking-at " ")
                       (forward-char))
                     (python-info-beginning-of-block-p))
                   (concat
                    (string-trim-right
                     (buffer-substring-no-properties
                      (point)
                      (save-excursion
                        (python-nav-end-of-block)
                        (while (looking-at "[\n ]*\\(except\\)")
                          (goto-char (match-beginning 1))
                          (python-nav-end-of-block))
                        (point))))
                    "\n"))
                  ((lispy-bolp)
                   (lispy--string-dwim
                    (lispy--bounds-c-toplevel)))
                  (t
                   (cond ((lispy-left-p))
                         ((lispy-right-p)
                          (backward-list))
                         (t
                          (error "Unexpected")))
                   (setq bnd (lispy--bounds-dwim))
                   (ignore-errors (backward-sexp))
                   (while (or (eq (char-before) ?.)
                              (eq (char-after) ?\())
                     (backward-sexp))
                   (setcar bnd (point))
                   (lispy--string-dwim bnd)))))
    (setq res (lispy--eval-python str))
    (if (and res (not (equal res "")))
        (lispy-message
         (replace-regexp-in-string
          "%" "%%" res))
      (lispy-message lispy-eval-error))))

(defun lispy--python-proc ()
  (let ((proc-name "Python Internal[lispy]"))
    (if (process-live-p proc-name)
        (get-process proc-name)
      (let ((python-shell-font-lock-enable nil)
            (inferior-python-mode-hook nil))
        (get-buffer-process
         (python-shell-make-comint
          (python-shell-calculate-command) proc-name nil t))))))

(defun lispy--eval-python (str &optional plain)
  "Eval STR as Python code."
  (let ((single-line-p (= (cl-count ?\n str) 0)))
    (unless plain
      (setq str (string-trim-left str))
      (when (and single-line-p
                 (string-match "\\`\\(\\(?:\\sw\\|\\s_\\|[][]\\)+\\) = " str))
        (setq str (concat str (format "; print (repr (%s))" (match-string 1 str))))))
    (let ((res
           (if (or single-line-p
                   (string-match "\n .*\\'" str))
               (python-shell-send-string-no-output
                str (lispy--python-proc))
             (if (string-match "\\`\\([\0-\377[:nonascii:]]*\\)\n\\([^\n]*\\)\\'" str)
                 (let* ((p1 (match-string 1 str))
                        (p2 (match-string 2 str))
                        (p1-output (python-shell-send-string-no-output
                                    p1 (lispy--python-proc))))
                   (concat
                    (if (string= p1-output "")
                        ""
                      (concat p1-output "\n"))
                    (lispy--eval-python p2)))
               (error "unexpected")))))
      (cond ((string-match "^Traceback.*:" res)
             (set-text-properties
              (match-beginning 0)
              (match-end 0)
              '(face error)
              res)
             (setq lispy-eval-error res)
             nil)
            ((equal res "")
             (setq lispy-eval-error "(ok)")
             "")
            (t
             res)))))

(defun lispy--python-array-to-elisp (array-str)
  "Transform a Python string ARRAY-STR to an Elisp string array."
  (split-string
   (substring array-str 1 -1)
   ", "
   t
   "u?'"))

(defun lispy-python-completion-at-point ()
  (cond ((looking-back "^\\(import\\|from\\) .*" (line-beginning-position))
         (let* ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (point)))
                (str
                 (format
                  "import jedi; script=jedi.Script(\"%s\",1,%d); [_x_.name for _x_ in script.completions()]"
                  line (length line)))
                (cands
                 (lispy--python-array-to-elisp
                  (lispy--eval-python str)))
                (bnd (bounds-of-thing-at-point 'symbol))
                (beg (if bnd (car bnd) (point)))
                (end (if bnd (cdr bnd) (point))))
           (list beg end cands)))
        (t
         (python-shell-completion-at-point (lispy--python-proc)))))

(defvar lispy--python-arg-key-re "\\`\\(\\(?:\\sw\\|\\s_\\)+\\) ?= ?\\(.*\\)\\'"
  "Constant regexp for matching function keyword spec.")

(defun lispy--python-args (beg end)
  (let (res)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (forward-sexp)
        (while (and (< (point) end)
                    (not (looking-at ",")))
          (forward-sexp))
        (push (buffer-substring-no-properties
               beg (point))
              res)
        (skip-chars-forward ", \n")
        (setq beg (point))))
    (nreverse res)))

(defun lispy--python-debug-step-in ()
  (let* ((p-ar-beg (point))
         (p-ar-end (save-excursion
                     (forward-list)
                     (point)))
         (p-fn-end (progn
                     (skip-chars-backward " ")
                     (point)))
         (p-fn-beg (progn
                     (backward-sexp)
                     (point)))
         (fn (buffer-substring-no-properties
              p-fn-beg p-fn-end))
         (args
          (lispy--python-args (1+ p-ar-beg) (1- p-ar-end)))
         (args-key (cl-remove-if-not
                    (lambda (s)
                      (string-match lispy--python-arg-key-re s))
                    args))
         (args-normal (cl-set-difference args args-key))
         (fn-data
          (json-read-from-string
           (substring
            (lispy--eval-python
             (format "import inspect, json; json.dumps (inspect.getargspec (%s))"
                     fn))
            1 -1)))
         (fn-args
          (mapcar #'identity (elt fn-data 0)))
         (fn-defaults
          (mapcar
           (lambda (x)
             (if (null x)
                 "None"
               (prin1-to-string x)))
           (elt fn-data 3)))
         (fn-alist
          (cl-mapcar #'cons
                     fn-args
                     (append (make-list (- (length fn-args)
                                           (length fn-defaults))
                                        nil)
                             fn-defaults)))
         (fn-alist-x fn-alist)
         dbg-cmd)
    (dolist (arg args-normal)
      (setcdr (pop fn-alist-x) arg))
    (dolist (arg args-key)
      (if (string-match lispy--python-arg-key-re arg)
          (let ((arg-name (match-string 1 arg))
                (arg-val (match-string 2 arg))
                arg-cell)
            (if (setq arg-cell (assoc arg-name fn-alist))
                (setcdr arg-cell arg-val)
              (error "\"%s\" is not in %s" arg-name fn-alist)))
        (error "\"%s\" does not match the regex spec" arg)))
    (when (memq nil (mapcar #'cdr fn-alist))
      (error "Not all args were provided: %s" fn-alist))
    (setq dbg-cmd
          (mapconcat (lambda (x)
                       (format "%s = %s" (car x) (cdr x)))
                     fn-alist
                     "; "))
    (if (lispy--eval-python dbg-cmd t)
        (lispy-goto-symbol fn)
      (goto-char p-ar-beg)
      (message lispy-eval-error))))

(defun lispy-goto-symbol-python (_symbol)
  (save-restriction
    (widen)
    (deferred:sync!
        (jedi:goto-definition))
    (unless (looking-back "def " (line-beginning-position))
      (jedi:goto-definition))))

(provide 'le-python)

;;; le-python.el ends here
