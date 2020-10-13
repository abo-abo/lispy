;; (setq package-user-dir
;;       (expand-file-name
;;        (format "~/.elpa/%s/elpa"
;;                (concat emacs-version (when (getenv "MELPA_STABLE") "-stable")))))
;; (package-initialize)
;; (setq package-archives
;;       (list (if (getenv "MELPA_STABLE")
;;                 '("melpa-stable" . "https://stable.melpa.org/packages/")
;;               '("melpa" . "http://melpa.org/packages/"))
;;             '("gnu" . "http://elpa.gnu.org/packages/")))


(add-to-list 'load-path default-directory)

;; Silence the loading message
(setq iedit-toggle-key-default nil)

(defun straight-reload-all ()
  (interactive)
  (let ((build-dir (expand-file-name "straight/build/" user-emacs-directory)))
    (dolist (pkg (delete "cl-lib" (delete ".." (delete "." (directory-files build-dir)))))
      (let* ((dir (expand-file-name pkg build-dir))
             (autoloads (car (directory-files dir t "-autoloads.el"))))
        (add-to-list 'load-path dir)
        (when autoloads
          (load autoloads t 'nomessage))))))
(straight-reload-all)
(message "load-path: %S" load-path)
