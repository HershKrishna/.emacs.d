(setq ring-bell-function 'ignore)

(split-window-horizontally)

(transient-mark-mode 0)

(require 'package)

(add-to-list 'package-archives
             (cons "melpa"
		   "https://melpa.org/packages/")
	     t)

(package-initialize)

(add-to-list 'load-path (expand-file-name "elisp/" (file-name-directory load-file-name)))
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode)
  :bind (:map paredit-mode-map (";" . nil)))

(global-set-key (kbd "<f5>") #'recompile)
(global-set-key (kbd "<f6>") #'compile)
(use-package modalka
  :ensure t
  :init (require 'modalka-custom))

(require 'odin-mode)

(use-package undo-tree :ensure t :bind ("C-z" . undo-tree-redo) ("C-/" . undo-tree-undo) ("C-x u" . undo-tree-visualize))

(use-package window-numbering :ensure t :init (window-numbering-mode 1))

(use-package company :ensure t :init (global-company-mode))

(use-package magit :ensure t :bind ("C-x g" . magit))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)


(use-package ido  :config
  (setq ido-auto-merge-delay-time 99999999)
  (setq ido-everywhere t)
  (setq ido-virtual-buffers t)
  (ido-mode))

(use-package smex :ensure t :bind ("M-x" . smex))

(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)

(add-to-list 'default-frame-alist '(fullscreen . maximized));maximize window on startup

(setq inhibit-startup-screen t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defun install-c-save-hook ()
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(defun delete-all-trailing-whitespace () (interactive) (delete-trailing-whitespace (point-min) (point-max)))

(add-hook 'before-save-hook 'delete-all-trailing-whitespace)

(use-package clang-format :ensure t :hook (c-mode . install-c-save-hook) (c++-mode . install-c-save-hook))

(use-package smartparens :ensure t :init (smartparens-global-mode) (sp-pair "'" nil :actions :rem))

(use-package ggtags :ensure t :hook (c-mode . ggtags-mode) (c++-mode . ggtags-mode))

;;My c style
(c-add-style "ragestyle"
             '("gnu"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (class-open . 0)         ; Guessed value
                (defun-block-intro . +)          ; Guessed value
                (defun-close . 0)        ; Guessed value
                (inclass . +)            ; Guessed value
                (statement . 0)                  ; Guessed value
                (statement-cont . -)     ; Guessed value
                (topmost-intro . 0)      ; Guessed value
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

(global-subword-mode 1)

(defun highlight-todos () (interactive)
       (font-lock-add-keywords
        nil
        '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\|HACK\\|FIXME(.*)\\|TODO(.*)\\|BUG()\\|NOTE(.*)\\|HACK(.*)\\):"
           1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'highlight-todos)

(global-auto-revert-mode t)
(use-package lua-mode :ensure t)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c-mode))

(use-package org :ensure t :bind (:map org-mode-map ("C-T" . org-todo)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(defun zig-fmt-buffer ()
  (interactive)
  (let ((temp-buffer (generate-new-buffer-name "*zig-fmt*" "*zig-fmt*"))
	(temp-file (make-temp-file "zig-fmt" nil ".err")))
    (let ((status (call-process-region nil nil "zig" nil (list temp-buffer temp-file) nil "fmt" "--stdin"))
	  (stderr
           (with-temp-buffer
             (unless (zerop (cadr (insert-file-contents temp-file)))
               (insert ": "))
             (buffer-substring-no-properties (point-min) (point-max)))))
      (cond
       ((stringp status)
        (error "(mycustom-fmt killed by signal %s%s)" status stderr))
       ((not (zerop status))
        (error "(mycustom-fmt failed with code %d%s)" status stderr))
       (t
	(replace-buffer-contents temp-buffer))))
    (delete-file temp-file)
    (kill-buffer temp-buffer)))


(defun zig-fmt-save-hook-for-this-buffer ()
  (add-hook
   'before-save-hook
   (lambda () (progn (zig-fmt-buffer)
		     ;; Continue to save.
		     nil))
   nil
   ;; Buffer local hook.
   t))

(add-hook 'zig-mode-hook
	  (lambda () (zig-fmt-save-hook-for-this-buffer)))

(defun zig-build () (interactive) (compilation-start "zig build"))
(defun zig-run () (interactive) (compilation-start "zig build run"))

(use-package zig-mode :ensure t)

(use-package linum-relative :ensure t :init (linum-relative-global-mode))

(global-set-key (kbd "<scroll>") (lambda () (interactive) nil))

(display-time-mode 1)

(use-package glsl-mode :ensure t)
