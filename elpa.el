(setq package-user-dir
      (expand-file-name (format ".cask/%s/elpa" emacs-version)))
(package-initialize)
(add-to-list 'load-path default-directory)
;; Silence the loading message
(setq iedit-toggle-key-default nil)

