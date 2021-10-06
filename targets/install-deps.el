(defconst lispy-dev-packages
  '(iedit
    multiple-cursors
    cider
    company
    spiral
    slime
    sly
    geiser
    clojure-mode
    swiper
    hydra
    ace-window
    helm
    projectile
    find-file-in-project
    undercover
    zoutline))

(defun package-install-packages (packages)
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
              '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-refresh-contents)

  (dolist (package packages)
    (if (package-installed-p package)
        (message "%S: OK" package)
      (condition-case nil
          (progn
            (package-install package)
            (message "%S: OK" package))
        (error
         (message "%S: FAIL" package)))))

  (save-window-excursion
    (package-list-packages t)
    (condition-case nil
        (progn
          (package-menu-mark-upgrades)
          (package-menu-execute t))
      (error
       (message "All packages up to date")))))

(defun straight-install-packages (packages)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (message "user-emacs-directory: %S" user-emacs-directory)
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (dolist (package packages)
    (straight-use-package package)))

(straight-install-packages lispy-dev-packages)
