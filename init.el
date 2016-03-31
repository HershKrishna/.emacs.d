;;Default colors so I don't have to deal with default if something fails
(set-foreground-color "#F08")
(set-background-color "#000")

;;set up ui directory to load files
(add-to-list 'load-path "~/.emacs.d/ui")

;;Package crap
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize) ;;make packages work from gnu marmalade and melpa

;;List of packages I use. Will grow over time of course
(package-install 'racket-mode)
(package-install 'cyberpunk-theme)
(package-install 'paredit)
(package-install 'faceup)

(desktop-save-mode 1);;Make sure session is preserved

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(ido-mode t);;set autocomplete in my file buffers

;;Set up ui stuff so I can deal with it in seperate files
(load-library "ui")
(require 'ui)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;Set up paredit mode
(add-hook 'emacs-lisp-mode-hook
	  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook
	  #'enable-paredit-mode)
(add-hook 'ielm-mode-hook
	  #'enable-paredit-mode)
(add-hook 'lisp-mode-hook
	  #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook
	  #'enable-paredit-mode)
(add-hook 'scheme-mode-hook
	  #'enable-paredit-mode)
(add-hook  'racket-mode-hook
	   #'enable-paredit-mode)
(add-hook 'racket-repl-mode-hook
	  #'enable-paredit-mode)
