
;; Keyboard macros

(global-set-key (kbd "C-=") 'kmacro-start-macro)
(global-set-key (kbd "C-!") 'kmacro-!)


;; Tab bar

(global-set-key (kbd "s-1") (lambda () (interactive) (tab-bar-select-tab 1)))
(global-set-key (kbd "s-2") (lambda () (interactive) (tab-bar-select-tab 2)))
(global-set-key (kbd "s-3") (lambda () (interactive) (tab-bar-select-tab 3)))
(global-set-key (kbd "s-4") (lambda () (interactive) (tab-bar-select-tab 4)))
(global-set-key (kbd "s-5") (lambda () (interactive) (tab-bar-select-tab 5)))
(global-set-key (kbd "s-6") (lambda () (interactive) (tab-bar-select-tab 6)))
(global-set-key (kbd "s-7") (lambda () (interactive) (tab-bar-select-tab 7)))
(global-set-key (kbd "s-8") (lambda () (interactive) (tab-bar-select-tab 8)))
(global-set-key (kbd "s-9") (lambda () (interactive) (tab-bar-select-tab 9)))


;; Mode-specific

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-&") 'async-shell-command-named)


;; Navigation

(global-set-key (kbd "C-p") 'avoid-this-key)
(global-set-key (kbd "C-h") 'previous-line)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "M-a") 'backward-up-list)
(global-set-key (kbd "M-a") 'up-list)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-\\") 'goto-last-point)
(global-set-key (kbd "C-,") 'quickjump-forward)
(global-set-key (kbd "C-m") 'quickjump-backward)
(global-set-key (kbd "RET") 'newline) ;; Order is important after C-m above.


;; Mouse

(global-set-key (kbd "<down-mouse-1>") 'mouse-drag-region)


;; Window operations

(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)
(global-set-key (kbd "C-z") 'ido-switch-buffer)


;; Kill ring

(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)


;; Buffer operations

(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-t") 'replace-string-or-query-replace)
(global-set-key (kbd "C-x C-k C-o") 'delete-blank-lines-in)
(global-set-key (kbd "M-Q") 'unfill-paragraph)


;; Unbinding keys

(global-set-key [f9] nil)
(global-set-key [f10] nil)
(global-set-key [f11] nil)
(global-set-key [f12] nil)
(global-set-key [f3] nil)
(global-set-key (kbd "C-x (") nil)
(global-set-key (kbd "C-x C-c") nil)


;; Key translations

(define-key key-translation-map (kbd "s-g") (kbd "C-g"))
(define-key input-decode-map "\C-m" [C-m])


;; Remaps

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))


;; Signals

(define-key special-event-map [sigusr1] 'usr1-handler)
;; test via (signal-process (emacs-pid) 'sigusr1)
