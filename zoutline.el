;;; zoutline.el --- Simple outline interface. -*- lexical-binding: t -*-

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

(require 'outline)

(defun zo-up (arg)
  "Move ARG times up by outline."
  (interactive "p")
  (let ((i 0)
        out)
    (ignore-errors
      (while (<= (cl-incf i) arg)
        (outline-backward-same-level 1)
        t
        (setq out t)))
    out))

(defun zo-down (arg)
  "Move ARG times down by outline.
Return the amount of times moved.
Return nil if moved 0 times."
  (interactive "p")
  (let ((pt 0)
        (i 0)
        (outline-ok t))
    (while (and outline-ok
                (<= (cl-incf i) arg)
                (> (point) pt))
      (setq pt (point))
      (condition-case nil
          (outline-forward-same-level 1)
        (error (setq outline-ok nil))))
    (cl-decf i)
    (unless (= 0 i)
      i)))

(defvar zo-lvl-re [nil
                   "\n\\* "
                   "\n\\*\\{2\\} "
                   "\n\\*\\{3\\} "
                   "\n\\*\\{4\\} "
                   "\n\\*\\{5\\} "
                   "\n\\*\\{6\\} "
                   "\n\\*\\{7\\} "])

(defun zo-down-visible (&optional arg)
  "Move ARG times down by outline."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((lvl (org-outline-level))
        res)
    (if (= lvl 1)
        (re-search-forward (aref zo-lvl-re lvl) nil t arg)
      (let ((end (save-excursion
                   (or (re-search-forward (aref zo-lvl-re (1- lvl)) nil t)
                       (point-max)))))
        (when (setq res (re-search-forward (aref zo-lvl-re lvl) end t arg))
          (reveal-post-command))))
    (when res
      (beginning-of-line)
      (point))))

(defun zo-left (arg)
  (outline-up-heading arg))

(defun zo-right-once ()
  (let ((pt (point))
        (lvl-1 (funcall outline-level))
        lvl-2)
    (if (outline-next-heading)
        (progn
          (setq lvl-2 (funcall outline-level))
          (if (> lvl-2 lvl-1)
              (progn
                (goto-char pt)
                nil)
            1))
      (goto-char pt)
      nil)))

(defun zo-right (arg)
  (let ((i 0))
    (while (and (< i arg)
                (zo-right-once))
      (cl-incf i))
    i))

(provide 'zoutline)
