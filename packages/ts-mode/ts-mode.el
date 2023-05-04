(define-derived-mode ts-mode
   prog-mode "TS"
   "Major mode for TypeScript.
 \\{ts-mode-map}"
   (set (make-local-variable 'font-lock-defaults) '(ts-keywords t nil nil))
   (setq-local comment-start "// ")
   (setq-local comment-start-skip "// ")
   (setq-local comment-end ""))

(defconst ts-keywords
  `((" // .*$" . font-lock-comment-face)
    ("/\\*.*?\\*/" . font-lock-comment-face)
    (" //$" . font-lock-comment-face)
    ("^//.*" . font-lock-comment-face)
    ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
    ("\\<\\*?\\[?[A-Z][a-zA-Z0-9_']*\\]?\\>" . font-lock-type-face)
    (,(concat "\\<" (regexp-opt '("return" "if" "else" "case" "switch" "import" "as" "export" "function" "type" "class" "const" "?" "let")) "\\>") . font-lock-keyword-face)
    (,(regexp-opt '("?" "=>" "===" "||" "&&" ";")) . font-lock-builtin-face)
    ("^[a-zA-Z_'0-9]+ " . font-lock-function-name-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ts\\'" . ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . ts-mode))

(provide 'ts-mode)
