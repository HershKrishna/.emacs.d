;; -*- lexical-binding: t -*-

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

(use-package undo-tree :ensure t :bind ("C-z" . undo-tree-redo) ("C-/" . undo-tree-undo) ("C-x u" . undo-tree-visualize) :init (global-undo-tree-mode))

(use-package window-numbering :ensure t :init (window-numbering-mode 1))

(use-package company :ensure t :init (global-company-mode) :bind ("M-i" . company-complete))

(use-package magit :ensure t :bind ("C-x g" . magit) ("C-x f" . magit-blame))

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

(use-package glsl-mode :ensure t)
(use-package clang-format :ensure t :hook (c-mode . install-c-save-hook) (c++-mode . install-c-save-hook) (glsl-mode . install-c-save-hook) (ispc-mode . install-c-save-hook))

(use-package smartparens :ensure t :init (smartparens-global-mode) (sp-pair "'" nil :actions :rem))

(use-package ggtags :ensure t :hook (c-mode . ggtags-mode) (c++-mode . ggtags-mode))

;;My c style
(c-add-style "ragestyle"
             '("gnu"
               (c-basic-offset . 2)     ; Guessed value
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
        '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\|HACK\\|STUDY\\|FIXME(.*)\\|TODO(.*)\\|BUG()\\|NOTE(.*)\\|HACK(.*)\\|STUDY\\):"
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


(use-package org :ensure t :bind (:map org-mode-map ("C-T" . org-todo)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(defun zig-fmt-buffer ()
  (interactive)
  (let ((temp-buffer (generate-new-buffer-name "*zig-fmt*"))
	(temp-file (make-temp-file "zig-fmt" nil ".err")))
    (let ((status (call-process-region nil nil "zig" nil (list temp-buffer temp-file) nil "fmt" "--stdin"))
	  (stderr
           (with-temp-buffer
             (unless (zerop (cadr (insert-file-contents temp-file)))
               (insert ": "))
             (buffer-substring-no-properties (point-min) (point-max)))))
      (cond
       ((stringp status)
        (delete-file temp-file)
        (kill-buffer temp-buffer)
        (error "(mycustom-fmt killed by signal %s%s)" status stderr))
       ((not (zerop status))
        (delete-file temp-file)
            (kill-buffer temp-buffer)
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

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
   :bind (:map markdown-mode-map ("M-n" . markdown-forward-paragraph) ("M-p" . markdown-backward-paragraph)))

(use-package rainbow-delimiters :ensure t :hook (prog-mode . rainbow-delimiters-mode))

(use-package hideshow
  :ensure t
  :bind ("C-c @ C-c" . hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode))
(server-start)
(setq default-buffer-file-coding-system 'utf-8-unix)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(use-package lsp-mode :ensure t)

(require 'lsp)
(add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
(lsp-register-client
  (make-lsp-client
    :new-connection (lsp-stdio-connection "zls")
    :major-modes '(zig-mode)
    :server-id 'zls))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))


(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))


(eval-and-compile
  ;; Make our mode known to the language constant system.  Use C
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'ispc-mode 'c++-mode))

(defconst ispc-font-lock-keywords-1
  (c-lang-const c-matchers-1 ispc)
  "Minimal highlighting for ISPC mode.")

(defconst ispc-font-lock-keywords-2
  (c-lang-const c-matchers-2 ispc)
  "Fast normal highlighting for ISPC mode.")

(defconst ispc-font-lock-keywords-3
  (c-lang-const c-matchers-3 ispc)
  "Accurate normal highlighting for ISPC mode.")

(defvar ispc-mode-syntax-table nil
  "Syntax table used in cuda-mode buffers.")
(or ispc-mode-syntax-table
    (setq ispc-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table ispc))))

(defvar ispc-mode-abbrev-table nil
  "Abbreviation table used in ispc-mode buffers.")

(c-define-abbrev-table 'ispc-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar ispc-mode-map (let ((map (c-make-inherited-keymap)))
              ;; Add bindings which are only useful for ISPC
              map)
  "Keymap used in ispc-mode buffers.")

(easy-menu-define ispc-menu ispc-mode-map "ISPC Mode Commands"
          ;; Can use `ispc' as the language for `c-mode-menu'
          ;; since its definition covers any language.  In
          ;; this case the language is used to adapt to the
          ;; nonexistence of a cpp pass and thus removing some
          ;; irrelevant menu alternatives.
          (cons "ISPC" (c-lang-const c-mode-menu ispc)))

(defun ispc-mode ()
  "Major mode for editing ISPC. ISPC is a C like language
extension for vector coding created by Intel

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `ispc-mode-hook'.

Key bindings:
\\{ispc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table ispc-mode-syntax-table)
  (setq major-mode 'ispc-mode
        mode-name "ispc"
        local-abbrev-table ispc-mode-abbrev-table
        abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars ispc-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'ispc-mode)
  (easy-menu-add ispc-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'ispc-mode-hook)
  (setq font-lock-keywords-case-fold-search t)
  (c-update-modeline))


