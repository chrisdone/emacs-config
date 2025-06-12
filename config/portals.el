(add-hook 'portal-out-mode-hook 'my-portal-out-hook)

(defun my-portal-out-hook ()
  (portal-ansi-colors-minor-mode)
  (auto-revert-tail-mode)
  (goto-char (point-max))
  (push-mark (point-max)))

(define-key portal-out-mode-map (kbd "RET") 'my-portal-out-ret)

(defun my-portal-out-ret ()
  "Jumps to filename in the contextual directory in which the process was ran."
  (interactive)
  (let ((default-directory (portal-read-json-file portal-out-portal "directory")))
    (let ((file-name-at-point (ffap-file-at-point)))
      (when file-name-at-point
        (let ((line (save-excursion
                      (goto-char (line-beginning-position))
                      (forward-char (length file-name-at-point))
                      (when (looking-at ":\\([0-9]+\\)")
                        (string-to-number (match-string-no-properties 1))))))
          (find-file-other-window file-name-at-point)
          (when line
            (goto-line line)
            (back-to-indentation)))))))
