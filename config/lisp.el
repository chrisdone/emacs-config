
;; Fundamental functions

(defun emacs-lisp-expand-clever (arg)
  "Cleverly expand symbols with normal dabbrev-expand, but also
  if the symbol is -foo, then expand to module-name-foo."
  (interactive "*P")
  (let ((sym (dabbrev--abbrev-at-point)))
    (cond
     ((string-match "^-" sym)
      (save-excursion
        (backward-sexp 1)
        (insert (emacs-lisp-module-name)))
      (dabbrev-expand arg))
     (t (dabbrev-expand arg)))))

(defun emacs-lisp-module-name ()
  "Search the buffer for `provide' declaration."
  (save-excursion
    (search-forward-regexp "^(provide '")
    (symbol-name (symbol-at-point))))


;; Keybindings

(define-key emacs-lisp-mode-map (kbd "M-/") 'emacs-lisp-expand-clever)
