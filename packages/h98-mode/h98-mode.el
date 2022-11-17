(define-derived-mode h98-mode
   prog-mode "H98"
   "Major mode for Haskell 98.
 \\{h98-mode-map}"
   (set (make-local-variable 'font-lock-defaults) '(h98-keywords t nil nil))
   (setq-local comment-start "-- ")
   (setq-local comment-start-skip "-- ")
   (setq-local comment-end ""))

(defconst h98-keywords
  `((" -- .*$" . font-lock-comment-face)
    ("{-.*-}" . font-lock-comment-face)
    (" --$" . font-lock-comment-face)
    ("^--.*" . font-lock-comment-face)
    ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
    ("\\<\\*?\\[?[A-Z][a-zA-Z0-9_']*\\]?\\>" . font-lock-type-face)
    (,(concat "\\<" (regexp-opt '("deriving" "case" "of" "stock" "anyclass" "via" "import" "module" "qualified" "as" "class" "instance" "where" "data" "type" "newtype" "do" "let" "$")) "\\>") . font-lock-keyword-face)
    ("^[a-zA-Z_'0-9]+ " . font-lock-function-name-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hs\\'" . h98-mode))

(provide 'h98-mode)
