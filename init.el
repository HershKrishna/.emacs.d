;;set up ui directory to load files
(add-to-list 'load-path "~/.emacs.d/ui")
(add-to-list 'load-path "~/.installed-libs/dylan-mode")

(add-to-list 'load-path "~/.emacs.d/elisp")
;;Package crap
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize) ;;make packages work from gnu marmalade and melpa

;;List of packages I use. Will grow over time of course
(defvar my-packages '(paredit
		      magit
		      cider
		      slime
		      markdown-mode
		      rbenv
		      multiple-cursors
		      ggtags
                      rainbow-delimiters
                      undo-tree
                      window-numbering
                      company
                      column-enforce-mode
                      flycheck
                      modalka))


;;Install missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (print (format "Installing %s" p))
    (package-install p)))

;;Set backups out of my goddamn directories I am sick of this shit
(defvar user-temporary-file-directory
  "~/.emacs-backup")

;;; GDB stuff

(defadvice gdb  (after gdb-keybind)
  (define-key gud-minor-mode-map (kbd "<f10>") 'gud-next)
  (define-key gud-minor-mode-map (kbd "<f11>") 'gud-cont))
(defvar gud-overlay
    (let* ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'face 'secondary-selection)
      ov)
    "Overlay variable for GUD highlighting.")
(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    ;;(move-overlay ov (line-beginning-position) (line-end-position)
                    (current-buffer)))))
(defun gud-kill-buffer ()
  (interactive)
  (delete-overlay gud-overlay))
(add-hook 'kill-buffer-hook 'gud-kill-buffer)

(ido-mode t) ;;set autocomplete in my file buffers

;;Set up ui stuff so I can deal with it in seperate files
(load-library "ui")
(require 'ui)

;;Set up paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)

;;; Pianobar

(autoload 'pianobar "pianobar" nil t)
(add-hook 'pianobar-mode-hook (lambda ()
                                (global-set-key
                                 (kbd "M-`")
                                 'pianobar-play-or-pause)))
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

(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'paredit-mode)
(defadvice slime (after keydef)
 (define-key slime-repl-mode-map (kbd "C-c l")
   'slime-hyperspec-lookup))

;;; Special thanks to Andy Moreton on the gnu.emacs.help list for the
;;; following code This code makes lookup go to a page in w3m-mode
;;; rather than in the system web browser
(defadvice common-lisp-hyperspec (around common-lisp-hyperspec/w3m activate)
	   "Use w3m to lookup symbols in the Common Lisp HyperSpec."
	   (let ((browse-url-browser-function 'w3m-browse-url))
	     ad-do-it))

;;; Imenu
(require 'imenu)
(defun my-imenu-rescan ()
  (interactive)
  (imenu--menubar-select imenu--rescan-item))
(global-set-key (kbd "<f7>") #'my-imenu-rescan)
(require 'helm)
(require 'sr-speedbar)
(global-set-key (kbd "<f8>") #'sr-speedbar-toggle)

;;C-mode-stuff
(defun c-hook ()
  (local-set-key (kbd "<f7>") 'recompile)
  (local-set-key (kbd "<f6>") 'realgud:gdb)
  (smartparens-mode 1)
  (subword-mode))
(defun highlight-todos () (interactive)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1
                             font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'highlight-todos)
(add-hook 'c-mode-common-hook
          'c-hook)
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

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
(add-hook 'racket-mode-hook 'paredit-mode)

(add-hook 'racket-mode-hook
	  (lambda ()
	    (define-key racket-mode-map (kbd "<f6>")  'racket-open-in-drracket)
            (font-lock-add-keywords
		  nil
		  '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1
                     font-lock-warning-face t)))))
(add-hook 'racket-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'racket-repl-mode-hook 'paredit-mode)

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

;;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(server-start)
(ignore-errors (load (expand-file-name "~/quicklisp/slime-helper.el")))


(add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
      slime-repl-prompt-face
      rear-nonsticky
      (slime-repl-prompt read-only font-lock-face intangible)))))
(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun slime-repl-font-lock-setup ()
  (setq font-lock-defaults
        '(slime-repl-font-lock-keywords
         ;; From lisp-mode.el
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-syntactic-face-function
         . lisp-font-lock-syntactic-face-function))))

(window-numbering-mode 1)

(defun remove-tabs ()
  "I don't much like tabs. Emacs won't insert them unless it *has* too but
here's a way to remove them in files that have them"
  (interactive)
  (untabify (point-min) (point-max)))


(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-c n") 'my-dired-create-file)
     (defun my-dired-create-file (file)
       "Create a file called FILE.
If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c <right>") 'hs-show-block)
(global-set-key (kbd "C-c <left>") 'hs-hide-block)

(column-number-mode)
(setq backup-directory-alist '(("." . "~/backup")))
(add-hook 'geiser-mode-hook #'paredit-mode)


(require 'cider)
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
(add-hook 'clojure-mode-hook
 'paredit-mode)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")



(setq gdb-non-stop-settig nil)
(autoload 'realgud:gdb "realgud.el")

(setq ido-auto-merge-work-directories-length -1)
(setq ido-create-new-buffer 'always)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "M-i") 'pop-to-mark-command)

(autoload 'toggle-source "toggle.el")

(global-set-key (kbd "<f4>") 'toggle-source)

(defun realgud-short-hook ()
  (define-key realgud:gdb-short-key-mode-map
    (kbd "x") 'realgud:cmd-clear))
(add-hook 'realgud-short-key-mode-hook 'realgud-short-hook)

(require 'flycheck)

(global-flycheck-mode)
(global-set-key (kbd "C-j") 'electrify-return-if-match)


(global-set-key (kbd "C-M-n") 'next-error)
(global-set-key (kbd "C-M-p") 'previous-error)
;;; Ada-mode
(defun ada-setup ()
  (interactive)
  (flycheck-mode -1)
  (smartparens-mode 1))
(add-hook 'ada-mode-hook 'ada-setup)
