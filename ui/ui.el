(provide 'ui)

(set-foreground-color "#23F")
(set-background-color "#CDD")
(set-cursor-color "#23F")

;;Font settings
(set-face-attribute 'default nil :font "Liberation Mono-9")

(require 'linum)
(global-linum-mode 1);;Set up Emacs so every line shows its number

(blink-cursor-mode 0);;screw whoever though blinking cursors were a good idea

(tool-bar-mode -1)
(toggle-scroll-bar -1)

(global-font-lock-mode 0)

;;set up multiple cursor interaction
(require 'multiple-cursors)

(global-set-key (kbd "C-x C-m") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
