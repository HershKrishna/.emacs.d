;;set up ui directory to load files
(add-to-list 'load-path "~/.emacs.d/ui")

;;Package crap
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize) ;;make packages work from gnu marmalade and melpa

;;List of packages I use. Will grow over time of course
(defvar my-packages '(paredit
		      magit
		      cider
		      slime
		      markdown-mode
		      rbenv))

;;Install missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (print (format "Installing %s" p))
    (package-install p)))

;;Set backups out of my goddamn directories I am sick of this shit
(defvar user-temporary-file-directory
  "~/.emacs-backup")

(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist 
      `(("." . ,user-temporary-file-directory)
	(,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))


(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


(ido-mode t);;set autocomplete in my file buffers

;;Set up ui stuff so I can deal with it in seperate files
(load-library "ui")
(require 'ui)

;;Set up paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)

;;; Pianobar

(autoload 'pianobar "pianobar" nil t)
(add-hook 'pianobar-mode-hook
	  (lambda ()
	    (global-set-key (kbd "<f7>") 'pianobar-play-or-pause)
	    (global-set-key (kbd "<f8>") 'pianobar-love-current-song)
	    (global-set-key (kbd "<f9>") 'pianobar-ban-current-song)))

;;Function I needed once from EmacsWiki. Might as well keep it.
(defun kill-other-buffers ()
  "kill all the other buffers"
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-l") 'backward-kill-word)


;;Common lisp stuff
(require 'slime)
(define-key slime-mode-map (kbd "C-c l")
  'slime-hyperspec-lookup)

;;; Special thanks to Andy Moreton on the gnu.emacs.help list for the following code
;;; This code makes lookup go to a page in w3m-mode rather than in the system web browser
(defadvice common-lisp-hyperspec (around common-lisp-hyperspec/w3m activate)
	   "Use w3m to lookup symbols in the Common Lisp HyperSpec."
	   (let ((browse-url-browser-function 'w3m-browse-url))
	     ad-do-it))

;;C-mode-stuff
(defun c-hook ()
  (local-set-key (kbd "<f5>") 'compile)
  (local-set-key (kbd "<f6>") 'gdb)
  (smartparens-mode 1))

(add-hook 'c-mode-hook
	  'c-hook)
(add-hook 'c++-mode-hook
	  'c-hook)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches 'electrify-return-match
   indent it"
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(global-set-key (kbd "RET") 'electrify-return-if-match)
;; Racket mode
(defun racket-open-in-drracket ()
  "Open the current file in Dr. Racket for debugging or testing purposes."
  (interactive)
  (start-process "drracket" nil "drracket" (buffer-file-name (current-buffer)) ))

(add-hook 'racket-mode-hook
	  (lambda ()
	    (define-key racket-mode-map (kbd "<f6>")  'racket-open-in-drracket)))
;; undo tree

(global-set-key (kbd "C-M-\\") 'undo-tree-visualize)

;;; Custom stuff *really* shouldn't live in init.el
(setq custom-file "~/.emacs-cust.el")
(if (not (file-exists-p custom-file))
    (with-temp-buffer (write-file custom-file)))
(load custom-file)

;;;Set up SBCL to work with SLIME


(setq inferior-lisp-program "/usr/local/bin/sbcl")


;;; global undo tree is awesome. We're going to use it

(global-undo-tree-mode)

;;; browser settings
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
