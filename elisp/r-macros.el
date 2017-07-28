(provide 'r-macros)

(fset 'yank-and-lint
   [?\C-y ?\M-x ?f ?o ?r backspace backspace backspace ?i ?n ?d ?e ?n ?d backspace ?t ?- ?r ?e ?g ?i ?o ?n return])

(global-set-key (kbd "C-M-y") 'yank-and-lint)
