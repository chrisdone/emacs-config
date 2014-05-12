(require 'markdown-mode)

(defvar markdown-code-languages
  '("haskell" "lisp" "javascript"))

(defun markdown-code-fence (beg end)
  "Make a code fence of the given region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (goto-char (line-beginning-position))
    (insert "``` "
            (ido-completing-read "Language: " markdown-code-languages)
            "\n")
    (goto-char end)
    (goto-char (line-end-position))
    (newline)
    (insert "```")))
