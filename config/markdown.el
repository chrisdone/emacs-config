(require 'markdown-mode)

(defvar markdown-code-languages
  '("haskell" "lisp" "javascript" "c" "yaml"))

(defun markdown-code-fence (beg end)
  "Make a code fence of the given region."
  (interactive "r")
  (let ((lang (ido-completing-read "Language: " markdown-code-languages)))
    (save-excursion
      (goto-char end)
      (goto-char (line-end-position))
      (newline)
      (insert "```")
      (goto-char beg)
      (goto-char (line-beginning-position))
      (insert "``` "
              lang
              "\n")
      (goto-char beg))))

(define-key markdown-mode-map (kbd "C-c C-f") 'markdown-code-fence)
(define-key markdown-mode-map (kbd "M-;") 'markdown-blockquote-region)

(define-key markdown-mode-map [f5] 'markdown-gist-refresh)
(defun markdown-gist-refresh ()
  (interactive)
  (save-buffer)
  (shell-command-to-string "git add .; git commit -m emacs-refresh; git push"))

(define-key markdown-mode-map (kbd "C-v C-n") 'flyspell-goto-next-error)
