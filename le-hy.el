;;; le-hy.el --- lispy support for Hy. -*- lexical-binding: t -*-

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

(require 'hy-mode nil t)
(require 'inf-lisp)

(defun lispy--hy-proc ()
  (let ((proc-name "hy"))
    (if (process-live-p proc-name)
        (get-process proc-name)
      (get-buffer-process
       (make-comint proc-name "hy")))))

(defun lispy--comint-eval (command)
  "Collect output of COMMAND without changing point."
  (let ((command-output-begin nil)
        (str nil)
        (last-cmd nil)
        (last-cmd-with-prompt nil)
        (buffer (process-buffer (lispy--hy-proc))))
    (with-current-buffer buffer
      (let ((inhibit-field-text-motion t)
            (inhibit-read-only t))
        ;; save the last command and delete the old prompt
        (beginning-of-line)
        (setq last-cmd-with-prompt
              (buffer-substring (point) (line-end-position)))
        (setq last-cmd (replace-regexp-in-string
                        "=> " "" last-cmd-with-prompt))
        (delete-region (point) (line-end-position))
        ;; send the command
        (setq command-output-begin (point))
        (comint-simple-send (get-buffer-process (current-buffer))
                            command)
        ;; collect the output
        (while (null (save-excursion
                       (let ((inhibit-field-text-motion t))
                         (goto-char command-output-begin)
                         (re-search-forward "^[. ]*=> \\s-*$" nil t))))
          (accept-process-output (get-buffer-process buffer))
          (goto-char (point-max)))
        (goto-char (point-max))
        (when (looking-back "^[. ]*=> *" (line-beginning-position))
          (goto-char (1- (match-beginning 0))))
        ;; save output to string
        (setq str (buffer-substring-no-properties command-output-begin (point)))
        ;; delete the output from the command line
        (delete-region command-output-begin (point-max))
        ;; restore prompt and insert last command
        (goto-char (point-max))
        (comint-send-string (get-buffer-process (current-buffer)) "\n")
        (insert last-cmd)
        ;; return the shell output
        str))))

(defun lispy--eval-hy (str)
  "Eval STR as Hy code."
  (let ((res (lispy--comint-eval str)))
    (if (member res '("" "\n"))
        "(ok)"
      res)))

(provide 'le-hy)

;;; le-hy.el ends here
