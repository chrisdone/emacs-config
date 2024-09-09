(define-derived-mode hell-mode
   prog-mode "Hell"
   "Major mode for Hell.
 \\{hell-mode-map}"
   (set (make-local-variable 'font-lock-defaults) '(hell-keywords t nil nil))
   (setq-local comment-start "-- ")
   (setq-local comment-start-skip "-- ")
   (setq-local comment-end ""))

(defconst hell-keywords
  `((" -- .*$" . font-lock-comment-face)
    ("{-.*-}" . font-lock-comment-face)
    (" --$" . font-lock-comment-face)
    ("^--.*" . font-lock-comment-face)
    ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
    ("\\<\\*?\\[?[A-Z][a-zA-Z0-9_']*\\]?\\>" . font-lock-type-face)
    (,(concat "\\<" (regexp-opt '("if" "then" "else" "data" "do" "let" "$" ".")) "\\>") . font-lock-keyword-face)
    ("^[a-zA-Z_'0-9]+ " . font-lock-function-name-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hell\\'" . hell-mode))

(provide 'hell-mode)
