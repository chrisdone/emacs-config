
;; Functions

(defun god-update-cursor ()
  "Update my cursor."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))


;; Keybindings

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "<escape>") 'god-local-mode)

(define-key god-local-mode-map (kbd "z") 'ido-switch-buffer)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "[") 'paredit-open-square)
(define-key god-local-mode-map (kbd "]") 'paredit-close-square)


;; Hooks

(add-hook 'god-mode-enabled-hook 'god-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-update-cursor)
