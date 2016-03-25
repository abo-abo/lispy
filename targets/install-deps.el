(setq package-user-dir
      (expand-file-name (format ".cask/%s/elpa" emacs-version)))
(message "installing in %s ...\n" package-user-dir)
(package-initialize)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents)

(defconst lispy-dev-packages
  '(iedit
    multiple-cursors
    cider
    slime
    geiser
    clojure-mode
    swiper
    hydra
    ace-window
    undercover))

(dolist (package lispy-dev-packages)
  (unless (package-installed-p package)
    (ignore-errors
     (package-install package))))

(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))

