(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'shell-mode-hook 'set-ansi-colors)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'magit-status-mode-hook 'turn-off-linum-mode)
