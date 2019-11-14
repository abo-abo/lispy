(setq package-user-dir
      (expand-file-name
       (format "~/.elpa/%s/elpa"
               (concat emacs-version (when (getenv "MELPA_STABLE") "-stable")))))
(package-initialize)
(setq package-archives
      (list (if (getenv "MELPA_STABLE")
                '("melpa-stable" . "https://stable.melpa.org/packages/")
              '("melpa" . "http://melpa.org/packages/"))
            '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'load-path default-directory)
;; Silence the loading message
(setq iedit-toggle-key-default nil)
