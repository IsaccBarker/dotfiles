(defun do-pretty ()
    (set-frame-font (concat font " " (number-to-string fontsize)) nil t)
    ;; Load theme of the week
    (require 'kaolin-themes)
    (load-theme 'kaolin-dark t)
    ;; Disable menu bar, scroll bar, and tool bar
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    ;; Display extra whitespace at the end of lines
	(add-hook 'prog-mode-hook
		(if (not (string= (frame-parameter nil 'name) "terminal")) (setq show-trailing-whitespace t)))
    ;;(setq-default show-trailing-whitespace t)
    ;; Add a line at column 100
    (setq-default display-fill-column-indicator-column 100)
    (global-display-fill-column-indicator-mode 1))

(defun do-qol()
    ;; Tab behaviour I actually want
    (setq-default indent-tabs-mode nil)
    (setq-default tab-width 4)
    (setq-default indent-line-function 'insert-tab)
    ;; Open ~/Developer if it exists, otherwise go to the home directory
    (cond ((file-directory-p "~/Developer") (setq initial-buffer-choice "~/Developer"))
          (t (setq initial-buffer-choice "~")))
    (setq compile-command "make -j"))

;; START PACKAGE SETUP ZONE
;; All packages should have a matching function named "setup-<package_name>". It will be
;; automatically run (Lisp is neat!). Time will tell if this is a good idea or not.

(defun setup-evil ()
    (evil-mode 1)
	(evil-ex-define-cmd "refresh-packages" 'refresh-packages))

;;(Defunsetup-centaur-tabs ()
;;    (centaur-tabs-mode t)
;;    (centaur-tabs-headline-match)
;;    (centaur-tabs-change-fonts font (* fontsize 10)) ;; Need to multiply by 10 for some reason.
;;    (setq centaur-tabs-set-icons t)
;;    (setq centaur-tabs-style "bar")
;;    (setq centaur-tabs-set-modified-marker t)
;;    (setq centaur-tabs-modified-marker "•"))

(defun setup-vterm ()
  (setq-default vterm-shell (executable-find "zsh")))

(defun setup-highlight-quoted() (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))
(defun setup-highlight-defined() (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))
(defun setup-highlight-numbers() (add-hook 'prog-mode-hook 'highlight-numbers-mode))
(defun setup-kaolin-themes ())
(defun setup-all-the-icons ())

;; END PACKAGE SETUP ZONE

(defun setup-package-archive ()
	(require 'package)
	(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
	(package-initialize)
	(package-refresh-contents))

(defun get-packages ()
    (dolist (package packages)
        (when (not (package-installed-p package))
            (package-install package))))

(defun setup-all-packages ()
    ;; Run any package setup function that might exist.
    (dolist (package packages)
        (require package)
        (setq setup-func-name (concat "setup-" (symbol-name package)))
        (setq setup-func (intern setup-func-name))
        (if (not (boundp setup-func)) (funcall setup-func))))

(defun refresh-packages ()
	(interactive)
	(setup-package-archive)
	(get-packages)
    (setup-all-packages))

(defun main ()
  (setq-default packages (list 'evil 'kaolin-themes 'all-the-icons 'highlight-quoted
                               'highlight-defined 'highlight-numbers 'vterm))
    (setq-default font "Ubuntu Mono")
    (setq-default fontsize 16)
	;; To setup and get new packages, use the refresh-packages command I've provided
	(setup-all-packages)
    (do-qol)
	(do-pretty))

(main)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm multi-compile all-the-icons highlight-numbers highlight-number highlight-defined highlight-quoted highlight-quotes kaolin-themes solarized-theme centaur-tabs monokai-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
