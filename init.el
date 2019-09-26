(setq visible-bell 1)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives 
	       (cons "melpa" (concat
			      proto
			      "://melpa.org/packages/")) t)

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-to-list 'load-path (expand-file-name "elisp/" (file-name-directory load-file-name)))
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode)
  :bind (:map paredit-mode-map (";" . nil)))

(use-package modalka
  :ensure t
  :init (require 'modalka-custom))

(use-package undo-tree :ensure t)

(use-package window-numbering :ensure t :init (window-numbering-mode 1))

(use-package company :ensure t :init (global-company-mode))

(use-package magit :ensure t :bind ("C-x g" . magit))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(ido-mode t)

(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)

(add-to-list 'default-frame-alist '(fullscreen . maximized));maximize window on startup

(setq inhibit-startup-screen t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defun install-c-save-hook ()
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(use-package clang-format :ensure t :hook (c-mode . install-c-save-hook) (c++-mode . install-c-save-hook))

(use-package smartparens :ensure t :init (smartparens-global-mode))

;;My c style
(c-add-style "ragestyle"
             '("gnu"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (class-open . 0)	 ; Guessed value
                (defun-block-intro . +)	 ; Guessed value
                (defun-close . 0)	 ; Guessed value
                (inclass . +)		 ; Guessed value
                (statement . 0)		 ; Guessed value
                (statement-cont . -)	 ; Guessed value
                (topmost-intro . 0)	 ; Guessed value
                (topmost-intro-cont . 0) ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont c-lineup-gcc-asm-reg 0)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-intro . c-lineup-arglist-intro-after-paren)
                (block-close . 0)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro . +)
                (brace-list-open . 0)
                (c . c-lineup-C-comments)
                (case-label . 0)
                (catch-clause . 0)
                (class-close . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (defun-open . 0)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . 0)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . c-lineup-inexpr-block)
                (inline-close . 0)
                (inline-open . 0)
                (inmodule . +)
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 5)
                (label . 0)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-block-intro . +)
                (statement-case-intro . +)
                (statement-case-open . 0)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (substatement-open . 0)
                (template-args-cont c-lineup-template-args +)))

             )

(setq c-default-style "ragestyle")

(split-window-horizontally)
(find-file "~/.emacs.d/init.el")
