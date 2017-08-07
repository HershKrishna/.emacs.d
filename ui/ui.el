;;; Package --- summary
;;; Commentary:

;;; Basic UI customization for my application

;;; Code:
;;Font settings
(set-face-attribute 'default nil :font "Source Code Pro-9" :height 100)

;; linum
(global-linum-mode)

(tool-bar-mode -1)
(toggle-scroll-bar -1)

;;set up multiple cursor interaction
(require 'multiple-cursors)

(global-set-key (kbd "C-x C-m") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;My c style
(c-add-style "ragestyle"
             '("gnu"
               (c-basic-offset . 2)     ; Guessed value
               (c-offsets-alist
                (block-close . 0)       ; Guessed value
                (brace-list-close . 0)  ; Guessed value
                (brace-list-intro . +)  ; Guessed value
                (brace-list-open . 0)   ; Guessed value
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (defun-open . 0)        ; Guessed value
                (statement . 0)             ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (substatement-open . 0)     ; Guessed value
                (topmost-intro . 0)         ; Guessed value
                (topmost-intro-cont . 0) ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont c-lineup-gcc-asm-reg 0)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-intro . c-lineup-arglist-intro-after-paren)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-entry . 0)
                (c . c-lineup-C-comments)
                (case-label . 0)
                (catch-clause . 0)
                (class-close . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (inclass . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
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
                (statement-case-intro . +)
                (statement-case-open . +)
                (statement-cont . +)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (template-args-cont c-lineup-template-args +))))

(setq c-default-style "ragestyle")

;;; Keybinding to make it easier to move around with more than two buffers

(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-set-key (kbd "C-x <up>") 'windmove-up)

(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-z") 'undo-tree-redo)

;;; transparency and other frame settings
(setq default-frame-alist '((fullscreen . maximized)))

;; set frame size
(set-frame-height (selected-frame) 90)
(set-frame-width (selected-frame) 200)

(require 'modalka)

(add-hook 'text-mode-hook 'modalka-mode)
(add-hook 'prog-mode-hook 'modalka-mode)

(modalka-define-kbd "p" "C-p")

(modalka-define-kbd "a" "C-a")
(modalka-define-kbd "e" "C-e")

(modalka-define-kbd "n" "C-n")

(modalka-define-kbd "f" "C-f")

(modalka-define-kbd "b" "C-b")

(modalka-define-kbd "s" "C-s")
(modalka-define-kbd "r" "C-r")

(modalka-define-kbd "R" "M-%")

(modalka-define-kbd "h" "M-h")

(modalka-define-kbd "g" "C-g")

(modalka-define-kbd "." "M-.")
(modalka-define-kbd "," "M-,")

(modalka-define-kbd "/" "C-/")

(modalka-define-kbd "w" "C-w")
(modalka-define-kbd "W" "M-w")

(modalka-define-kbd "y" "C-y")
(modalka-define-kbd "Y" "M-y")

(modalka-define-kbd "k" "C-k")

(modalka-define-kbd "o" "C-x o")

(modalka-define-kbd "d" "C-d")

(modalka-define-kbd "j" "C-j")

(modalka-define-kbd "B" "C-x b")

(modalka-define-kbd "q" "M-q")

(modalka-define-kbd "z" "C-z")

(modalka-define-kbd "l" "C-l")

(modalka-define-kbd "i" "M-i")

(modalka-define-kbd "u" "C-u")

(modalka-define-kbd "t" "C-t")

(modalka-define-kbd "x" "M-x")

(modalka-define-kbd "c" "M-c")

(modalka-define-kbd ";" "M-;")

(modalka-define-kbd "[" "C-x (")
(modalka-define-kbd "]" "C-x )")

(modalka-define-kbd "S" "C-x C-s")
(modalka-define-kbd "F" "C-x C-F")

(modalka-define-kbd "v" "C-v")

(modalka-define-kbd "m" "M-m")

(modalka-define-kbd "<SPC>" "C-<SPC>")

(global-set-key (kbd "C-i") #'modalka-mode)
(global-set-key (kbd "M-m") #'modalka-mode)


(setq-default cursor-type '(bar . 1))
(setq modalka-cursor-type 'box)

(appt-activate 1)
(display-time)
(split-window-horizontally)
(transient-mark-mode 0)
(provide 'ui)
;;; ui.el ends here
