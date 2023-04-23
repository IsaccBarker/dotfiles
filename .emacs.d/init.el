(defun do-pretty ()
    (set-frame-font (concat font " " (number-to-string fontsize)) nil t)
    ;; Disable menu bar, scroll bar, and tool bar
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    ;; Add a line at column 100
    (setq-default display-fill-column-indicator-column 100)
    (global-display-fill-column-indicator-mode 1)
    ;; Change window size and position
    (add-to-list 'default-frame-alist '(height . 48))
    (add-to-list 'default-frame-alist '(width . 120))
    (set-frame-position (selected-frame) 20 40)
    ;; Disable line numbers in certain modes but make them the default otherwise
    (column-number-mode)
    (global-display-line-numbers-mode t)
    (dolist (mode '(term-mode-hook vterm-mode-hook eshell-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 0))))
    ;; Disable evil mode in terminals and other places I don't want it. Evil mode has already been
    ;; set as the default in other places.
    (dolist (mode '(term-mode vterm-mode eshell-mode))
      (evil-set-initial-state mode 'emacs))
    ;; Disable showing extra whitespace at the end of lines in terminals but make it the
    ;; default otherwise.
    (setq-default show-trailing-whitespace t)
    (dolist (mode '(term-mode-hook vterm-mode-hook eshell-mode-hook))
        (add-hook mode (lambda ()
            (make-local-variable 'show-trailing-whitespace)
            (setq show-trailing-whitespace nil)))))

(defun setup-shell ()
    ;; Emacs's default shell is Bash, we want ZSH
    (setq zsh (executable-find "zsh"))
    (setq-default shell-file-name zsh)
    (setq-default explicit-shell-file-name zsh))

(defun setup-path ()
    ;; Make the emacs PATH variable match that of ZSHs.
    (interactive)
    (setq path-from-shell (shell-command-to-string (concat zsh " --login -c 'echo $PATH'")))
    (setq exec-path (split-string path-from-shell path-separator))
    (setenv "PATH" path-from-shell))

(defun setup-dashboard ()
    (setq dashboard-banner-logo-title
        "\"Emacs may stand for 'Editor MACroS', but with MyMacs, it stands for 'Extremely Marvelous And Convenient Stuff'!\"")
    (setq dashboard-startup-banner "~/.emacs.d/welcome.txt")
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-items '((recents  . 3)
                        (projects . 3)))
    (dashboard-setup-startup-hook))

(defun setup-projectile ()
    (require 'projectile)
    (when (file-directory-p "~/Developer")
        (setq projectile-project-search-path '("~/Developer")))
    (setq projectile-switch-project-action #'projectile-dired)
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

(defun setup-ivy ()
    (ivy-mode)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t))

(defun setup-all-packages ()
    ;; Org mode
    (use-package org)
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

    ;; Vim-like modal editing
    (straight-use-package '(evil :host github :repo "emacs-evil/evil"))
    (use-package evil
        :config (evil-mode t))

    ;; Virtual terminal
    (straight-use-package 'vterm)
    (use-package vterm
        :config (setq-default vterm-shell (executable-find "zsh")))

    ;; ThemeofTheDay
    (straight-use-package 'kaolin-themes)
    (use-package kaolin-themes
        :config (load-theme 'kaolin-dark t))

    ;; A dashboard that I'll configure to strip down
    (straight-use-package 'dashboard)
    (use-package dashboard
        :config (setup-dashboard))

    ;; Project management solution
    (straight-use-package 'projectile)
    (use-package projectile
        :diminish projectile-mode
        :config (setup-projectile))

    ;; Keybindings options panel
    ;; I can't memorize everything from the start
    (straight-use-package 'which-key)
    (use-package which-key
        :diminish which-key-mode
        :init (which-key-mode)
        :config (setq which-key-idle-delay 1))

    ;; Completion mechanism
    (straight-use-package 'ivy)
    (use-package ivy
        :config (setup-ivy))

    ;; Nice highlighting
    (straight-use-package 'highlight-quoted)
    (use-package highlight-quoted)
    (straight-use-package 'highlight-defined)
    (use-package highlight-defined)
    (straight-use-package 'highlight-numbers)
    (use-package highlight-numbers))

(defun main ()
    (setup-shell)
    (setup-path)
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
    (setq-default packages (list 'evil 'kaolin-themes 'highlight-quoted
                               'highlight-defined 'highlight-numbers 'vterm))
    (setq package-enable-at-startup nil)
    (defvar bootstrap-version)
    (setq-default font "Ubuntu Mono")
    (setq-default fontsize 16)
    (setup-all-packages)
    (do-pretty))

(main)
