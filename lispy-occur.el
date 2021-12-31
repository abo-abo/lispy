;;; lispy-occur.el --- Select a line within the current top level sexp. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021 Oleh Krehel

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
(require 'swiper)

(defcustom lispy-occur-backend 'ivy
  "Method to navigate to a line with `lispy-occur'."
  :type '(choice
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)))

(defvar lispy--occur-beg 1
  "Start position of the top level sexp during `lispy-occur'.")

(defvar lispy--occur-end 1
  "End position of the top level sexp during `lispy-occur'.")

(defun lispy--occur-candidates (&optional bnd)
  "Return the candidates for `lispy-occur'."
  (setq bnd (or bnd (save-excursion
                      (unless (and (bolp)
                                   (lispy-left-p))
                        (beginning-of-defun))
                      (lispy--bounds-dwim))))
  (let ((line-number -1)
        candidates)
    (setq lispy--occur-beg (car bnd))
    (setq lispy--occur-end (cdr bnd))
    (save-excursion
      (goto-char lispy--occur-beg)
      (while (< (point) lispy--occur-end)
        (push (format "%-3d %s"
                      (cl-incf line-number)
                      (buffer-substring
                       (line-beginning-position)
                       (line-end-position)))
              candidates)
        (forward-line 1)))
    (nreverse candidates)))

(defun lispy--occur-preselect ()
  "Initial candidate regex for `lispy-occur'."
  (format "^%d"
          (-
           (line-number-at-pos (point))
           (line-number-at-pos lispy--occur-beg))))

(defvar helm-input)
(declare-function helm "ext:helm")

(defun lispy-occur-action-goto-paren (x)
  "Goto line X for `lispy-occur'."
  (setq x (read x))
  (goto-char lispy--occur-beg)
  (let ((input (if (eq lispy-occur-backend 'helm)
                   helm-input
                 ivy-text))
        str-or-comment)
    (cond ((string= input "")
           (forward-line x)
           (back-to-indentation)
           (when (re-search-forward lispy-left (line-end-position) t)
             (goto-char (match-beginning 0))))

          ((setq str-or-comment
                 (progn
                   (forward-line x)
                   (re-search-forward (ivy--regex input)
                                      (line-end-position) t)
                   (lispy--in-string-or-comment-p)))
           (goto-char str-or-comment))

          ((re-search-backward lispy-left (line-beginning-position) t)
           (goto-char (match-beginning 0)))

          ((re-search-forward lispy-left (line-end-position) t)
           (goto-char (match-beginning 0)))

          (t
           (back-to-indentation)))))

(defun lispy-occur-action-goto-end (x)
  "Goto line X for `lispy-occur'."
  (setq x (read x))
  (goto-char lispy--occur-beg)
  (forward-line x)
  (re-search-forward (ivy--regex ivy-text) (line-end-position) t))

(defun lispy-occur-action-goto-beg (x)
  "Goto line X for `lispy-occur'."
  (when (lispy-occur-action-goto-end x)
    (goto-char (match-beginning 0))))

(defun lispy-occur-action-mc (_x)
  "Make a fake cursor for each `lispy-occur' candidate."
  (let ((cands (nreverse ivy--old-cands))
        cand)
    (while (setq cand (pop cands))
      (goto-char lispy--occur-beg)
      (forward-line (read cand))
      (re-search-forward (ivy--regex ivy-text) (line-end-position) t)
      (when cands
        (mc/create-fake-cursor-at-point))))
  (multiple-cursors-mode 1))

(ivy-set-actions
 'lispy-occur
 '(("m" lispy-occur-action-mc "multiple-cursors")
   ("j" lispy-occur-action-goto-beg "goto start")
   ("k" lispy-occur-action-goto-end "goto end")))

(defvar ivy-last)
(declare-function ivy-state-window "ext:ivy")

;;;###autoload
(defun lispy-occur ()
  "Select a line within current top level sexp.
See `lispy-occur-backend' for the selection back end."
  (interactive)
  (swiper--init)
  (cond ((eq lispy-occur-backend 'helm)
         (require 'helm)
         (add-hook 'helm-move-selection-after-hook
                   #'lispy--occur-update-input-helm)
         (add-hook 'helm-update-hook
                   #'lispy--occur-update-input-helm)
         (unwind-protect
              (helm :sources
                    `((name . "this defun")
                      (candidates . ,(lispy--occur-candidates))
                      (action . lispy-occur-action-goto-paren)
                      (match-strict .
                                    (lambda (x)
                                      (ignore-errors
                                        (string-match
                                         (ivy--regex helm-input) x)))))
                    :preselect (lispy--occur-preselect)
                    :buffer "*lispy-occur*")
           (swiper--cleanup)
           (remove-hook 'helm-move-selection-after-hook
                        #'lispy--occur-update-input-helm)
           (remove-hook 'helm-update-hook
                        #'lispy--occur-update-input-helm)))
        ((eq lispy-occur-backend 'ivy)
         (unwind-protect
              (ivy-read "pattern: "
                        (lispy--occur-candidates)
                        :preselect (lispy--occur-preselect)
                        :require-match t
                        :update-fn (lambda ()
                                     (lispy--occur-update-input
                                      ivy-text
                                      (ivy-state-current ivy-last)))
                        :action #'lispy-occur-action-goto-paren
                        :caller 'lispy-occur)
           (swiper--cleanup)
           (when (null ivy-exit)
             (goto-char swiper--opoint))))
        (t
         (error "Bad `lispy-occur-backend': %S" lispy-occur-backend))))

(defun lispy--occur-update-input-helm ()
  "Update selection for `lispy-occur' using `helm' back end."
  (lispy--occur-update-input
   helm-input
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun lispy--occur-update-input (input str)
  "Update selection for `ivy-occur'.
INPUT is the current input text.
STR is the full current candidate."
  (swiper--cleanup)
  (let ((re (ivy--regex input))
        (num (if (string-match "^[0-9]+" str)
                 (string-to-number (match-string 0 str))
               0)))
    (with-selected-window (ivy-state-window ivy-last)
      (goto-char lispy--occur-beg)
      (when (cl-plusp num)
        (forward-line num)
        (unless (<= (point) lispy--occur-end)
          (recenter)))
      (let ((ov (make-overlay (line-beginning-position)
                              (1+ (line-end-position)))))
        (overlay-put ov 'face 'swiper-line-face)
        (overlay-put ov 'window (ivy-state-window ivy-last))
        (push ov swiper--overlays))
      (re-search-forward re (line-end-position) t)
      (swiper--add-overlays
       re
       lispy--occur-beg
       lispy--occur-end))))

;;; lispy-occur.el ends here
