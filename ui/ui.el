(provide 'ui)

(set-foreground-color "#F63")
(set-background-color "#000")
(set-cursor-color "#23F")

;;Font settings
(set-face-attribute 'default nil :font "Liberation Mono-9" :height 90)

(require 'linum)
(global-linum-mode 1);;Set up Emacs so every line shows its number

(blink-cursor-mode 0);;screw whoever though blinking cursors were a good idea

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
	       (c-basic-offset . 2)	; Guessed value
	       (c-offsets-alist
		(block-close . 0)	; Guessed value
		(brace-list-close . 0)	; Guessed value
		(brace-list-intro . +)	; Guessed value
		(brace-list-open . 0)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(defun-close . 0)	; Guessed value
		(defun-open . 0)	; Guessed value
		(statement . 0)		    ; Guessed value
		(statement-block-intro . +) ; Guessed value
		(substatement-open . 0)	    ; Guessed value
		(topmost-intro . 0)	    ; Guessed value
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

