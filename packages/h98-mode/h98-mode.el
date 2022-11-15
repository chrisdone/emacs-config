(defconst h98-keywords
  `((" -- .*$" . font-lock-comment-face)
    ("{-.*-}" . font-lock-comment-face)
    (" --$" . font-lock-comment-face)
    ("^--.*" . font-lock-comment-face)
    ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
    ("\\<\\*?\\[?[A-Z][a-zA-Z]*\\]?\\>" . font-lock-type-face)
    (,(concat "\\<" (regexp-opt '("deriving" "stock" "anyclass" "via" "import" "module" "qualified" "as" "class" "instance" "where" "data" "type" "newtype" "do" "let" "$")) "\\>") . font-lock-keyword-face)
    ("^[a-zA-Z_']+ " . font-lock-function-name-face)))


(define-derived-mode h98-mode
   prog-mode "H98"
   "Major mode for Haskell 98.
 \\{h98-mode-map}"
   (set (make-local-variable 'font-lock-defaults) '(h98-keywords t nil nil))
   (setq-local comment-start "-- ")
   (setq-local comment-start-skip "-- ")
   (setq-local comment-end ""))

(provide 'h98-mode)
