(defun css-mode-curly ()
  (interactive)
  (when (not (looking-back " "))
    (insert " "))
  (insert "{"
          "\n")
  (save-excursion
    (insert "\n}"))
  (indent-for-tab-command))

(define-key css-mode-map (kbd "{") 'css-mode-curly)
