(provide 'ui)

(set-foreground-color "#F0F")
(set-background-color "#000")
(set-cursor-color "#FFF")

;;Font settings
(set-face-attribute 'default nil :font "Liberation Mono-9")

(require 'linum)
(global-linum-mode 1);;Set up Emacs so every line shows its number

(blink-cursor-mode 0);;screw whoever though blinking cursors were a good idea

(tool-bar-mode -1)
(toggle-scroll-bar -1)
