;; Initial version copied from Pawel Szulc

(define-derived-mode tla-mode fundamental-mode "tla"
  "Major Mode For Editing TLA language code."
  (setq font-lock-defaults '(tla-highlights))
  (font-lock-add-keywords nil '(("\\\\\\*.+" . font-lock-comment-face)))
  (font-lock-add-keywords nil '(("====.+" . font-lock-doc-face)))
  (font-lock-add-keywords nil '(("----.+" . font-lock-doc-face)))
  (font-lock-add-keywords nil '(("(\\*.+" . font-lock-doc-face)))
  (setq-local comment-start "\\* ")
  (setq-local comment-end ""))

(defconst tla-highlights
  `((,(concat
       "\\<"
       (regexp-opt
        (list "ASSUME" "UNCHANGED" "EXCEPT" "EXTENDS" "RECURSIVE" "CONSTANT" "VARIABLES" "VARIABLE" "SUBSET"))
       "\\>")
     . font-lock-keyword-face)
    (".+ ==" . font-lock-function-name-face)
    (,(regexp-opt
       (list "\\in" "\\cup" "\\A" "\\E" "/\\" "\\/" "->" ".." "="))
     . font-lock-keyword-face)
    (,(concat
       "\\<"
       (regexp-opt
        (list "TRUE" "FALSE" "Nat" "IF" "ELSE" "THEN"))
       "\\>")
     . font-lock-constant-face)))

(provide 'tla-mode)
