(define-key shell-mode-map (kbd "C-c C-k") 'erase-buffer)
(define-key comint-mode-map (kbd "RET") 'my-comint-send)
(define-key comint-mode-map (kbd "M-p") 'my-comint-prev)
