;; Reset the w3m mode map, because the default is very invasive. I
;; will hereon pick and choose exactly what keybindings w3m-mode
;; should have.
(setq w3m-mode-map (make-sparse-keymap))

;; Bindings I need
(define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
(define-key w3m-mode-map [f5] 'w3m-reload-this-page)
