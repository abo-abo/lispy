(setq package-user-dir
      (expand-file-name
       (format ".cask/%s/elpa"
               (concat emacs-version (when (getenv "MELPA_STABLE") "-stable")))))
(package-initialize)
(add-to-list 'load-path default-directory)
;; Silence the loading message
(setq iedit-toggle-key-default nil)
