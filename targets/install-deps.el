(setq melpa-stable (getenv "MELPA_STABLE"))
(setq package-user-dir
      (expand-file-name
       (format "~/.elpa/%s/elpa"
               (concat emacs-version (when melpa-stable "-stable")))))
(message "installing in %s ...\n" package-user-dir)
(package-initialize)
(setq package-archives
      (list (if melpa-stable
                '("melpa-stable" . "https://stable.melpa.org/packages/")
              '("melpa" . "http://melpa.org/packages/"))
            ;; '("gnu" . "http://elpa.gnu.org/packages/")
            ))
(package-refresh-contents)

(defconst lispy-dev-packages
  '(iedit
    multiple-cursors
    cider
    spiral
    slime
    sly
    geiser
    clojure-mode
    counsel
    hydra
    ace-window
    helm
    projectile
    find-file-in-project
    undercover
    zoutline))

(dolist (package lispy-dev-packages)
  (unless (package-installed-p package)
    (ignore-errors
     (package-install package))))

(save-window-excursion
  (package-list-packages t)
  (condition-case nil
      (progn
        (package-menu-mark-upgrades)
        (package-menu-execute t))
    (error
     (message "All packages up to date"))))
