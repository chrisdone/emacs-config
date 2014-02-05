
;; Fundamental functions

(defun emacs-lisp-expand-clever (arg)
  "Cleverly expand symbols with normal dabbrev-expand, but also
  if the symbol is -foo, then expand to module-name-foo."
  (interactive "*P")
  (let ((sym (dabbrev--abbrev-at-point)))
    (cond
     ((string-prefix-p "-" sym)
      (let ((namespace (emacs-lisp-module-name)))
        (when namespace
          (save-excursion
            (backward-sexp 1)
            (insert namespace))))
      (dabbrev-expand arg))
     (t (dabbrev-expand arg)))))

(defun emacs-lisp-module-name ()
  "Search the buffer for `provide' declaration."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "^(provide '" nil t 1)
      (symbol-name (symbol-at-point)))))


;; Keybindings

(define-key emacs-lisp-mode-map (kbd "M-/") 'emacs-lisp-expand-clever)
