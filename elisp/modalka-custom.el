;; -*- lexical-binding: t -*-
(modalka-global-mode)

(add-to-list 'modalka-excluded-modes 'magit-status-mode)
(add-to-list 'modalka-excluded-modes 'magit-popup-mode)
(add-to-list 'modalka-excluded-modes 'undo-tree-visualizer-mode)
(add-to-list 'modalka-excluded-modes 'comint-mode)
(add-to-list 'modalka-excluded-modes 'shell-mode)
(add-to-list 'modalka-excluded-modes 'compilation-mode)
(add-to-list 'modalka-excluded-modes 'help-mode)
(add-to-list 'modalka-excluded-modes 'calc-mode)
(add-to-list 'modalka-excluded-modes 'slime-repl-mode)
(add-to-list 'modalka-excluded-modes 'cider-repl-mode)

(defun replace-character-at-point (character)
  (interactive "cReplacement Character")
  (delete-char 1)
  (insert character)
  (backward-char))
(defun insert-character-at-point (character)
  (interactive "cInserted Character")
  (insert character)
  (backward-char))

(global-set-key (kbd "ESC M-d") #'replace-word)
(global-set-key (kbd "ESC M-r") #'replace-character-at-point)
(global-set-key (kbd "ESC M-i") #'insert-character-at-point)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(modalka-define-kbd "p" "C-p")

(modalka-define-kbd "a" "C-a")
(modalka-define-kbd "A" "C-x C-x")
(defun replace-word (x)
  (interactive "p")
  (kill-word x)
  (modalka-mode -1))
(modalka-define-kbd "e" "C-e")

(modalka-define-kbd "n" "C-n")

(modalka-define-kbd "f" "C-f")

(modalka-define-kbd "b" "C-b")

(modalka-define-kbd "s" "C-s")
(modalka-define-kbd "r" "C-r")

(modalka-define-kbd "Q" "M-%")
(modalka-define-kbd "R" "ESC M-d")

(modalka-define-kbd "h" "M-h")
(modalka-define-kbd "H" "C-c @ C-c")

(modalka-define-kbd "g" "C-g")
(define-key modalka-mode-map (kbd "G") #' goto-line)

(modalka-define-kbd "." "M-.")
(modalka-define-kbd "{"  "C-x 5 o")
(modalka-define-kbd "," "M-,")

(modalka-define-kbd "/" "C-/")

(modalka-define-kbd "w" "C-w")
(define-key modalka-mode-map (kbd "W") #'subword-mode)

(modalka-define-kbd "y" "C-y")
(modalka-define-kbd "Y" "M-y")

(modalka-define-kbd "k" "C-k")

(modalka-define-kbd "o" "C-o")
(modalka-define-kbd "O" "C-x o")

(modalka-define-kbd "K" "C-x k")

(modalka-define-kbd "d" "C-d")
(modalka-define-kbd "D" "C-x d")

(modalka-define-kbd "j" "C-j")

(modalka-define-kbd "B" "C-x b")

(modalka-define-kbd "q" "M-q")

(modalka-define-kbd "z" "C-z")

(modalka-define-kbd "l" "C-l")
(define-key modalka-mode-map "L" 'magit-blame)

(modalka-define-kbd "i" "ESC M-i")
(modalka-define-kbd "c" "ESC M-r")

(modalka-define-kbd "u" "C-u")
(modalka-define-kbd "U" "C-x u")

(modalka-define-kbd "t" "C-t")

(modalka-define-kbd "x" "M-x")
(modalka-define-kbd "X" "C-M-x")
(fset 'modalka-comment
      [?\C-\; ?\M-m])
(define-key modalka-mode-map ";" 'modalka-comment)

(defun save-all-buffers ()
  (interactive) (save-some-buffers t))

(define-key modalka-mode-map (kbd "S") 'save-all-buffers)
(modalka-define-kbd "F" "C-x C-f")

(modalka-define-kbd "v" "C-v")
(modalka-define-kbd "V" "M-v")
(modalka-define-kbd "T" "C-T")

(modalka-define-kbd "m" "M-m")

(modalka-define-kbd "<SPC>" "C-<SPC>")

(defun capitalize-letter ()
  (interactive)
  (capitalize-region (point-marker) (+ 1 (point-marker))))
(modalka-define-kbd "C" "<f5>")
(modalka-define-kbd "E" "<f6>")

(modalka-define-kbd ">" "M->")
(modalka-define-kbd "<" "M-<")

(define-key modalka-mode-map (kbd "N") 'next-error)
(define-key modalka-mode-map (kbd "P") 'previous-error)

(global-set-key (kbd "M-m") #'modalka-mode)

(defun  activate-and-set-mark ()
  "Push the mark and immediately activate it for a visible selection"
  (interactive)
  (push-mark)
  (activate-mark))
(define-key modalka-mode-map (kbd "M") #'activate-and-set-mark)

(global-set-key (kbd "M-[") #'kmacro-start-macro)
(global-set-key (kbd "M-]") #'kmacro-end-macro)
(define-key modalka-mode-map (kbd "J") (lookup-key global-map (kbd "C-x")))
(define-key modalka-mode-map (kbd "I") #'whitespace-mode)

(setq-default cursor-type '(bar . 1))
(setq modalka-cursor-type 'box)

(fset 'modalka-comment
      [?\M-\; ?\M-m])
(define-key modalka-mode-map ";" 'modalka-comment)

(global-set-key (kbd "C-h h") nil)

(provide 'modalka-custom)

