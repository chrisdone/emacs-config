
;; Requirements

(require 'haskell-mode)
(require 'haskell-simple-indent)
(require 'haskell-interactive-mode)
(require 'haskell-font-lock)
(require 'sgml-mode)
(require 'css-mode)


;; Functions

(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode: "
                              '("fundamental-mode"
                                "haskell-mode"
                                "espresso-mode"
                                "ghc-core-mode"
                                "org-mode")))))

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defun haskell-process-cabal-build-and-restart ()
  "Build and restart the Cabal project."
  (interactive)
  (cond
   (t
    (haskell-process-cabal-build)
    (haskell-process-queue-without-filters
     (haskell-process)
     (format ":!cd %s && scripts/restart\n" (haskell-session-cabal-dir (haskell-session)))))
   (t (turbo-devel-reload))))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (cond
     ;; Use grep
     (nil (let ((buffer
                 (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                    (haskell-session-current-dir (haskell-session))
                                    sym))))
            (with-current-buffer buffer
              (rename-buffer "*who-calls*")
              (switch-to-buffer-other-window buffer))))
     (t (ag sym (haskell-session-current-dir (haskell-session)))))))

(defun haskell-auto-insert-module-template ()
  "Insert a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module " (haskell-guess-module-name) " where"
     "\n"
     "\n")
    (goto-char (point-min))
    (forward-char 5)))

(defun shm-contextual-space ()
  "Do contextual space first, and run shm/space if no change in
the cursor position happened."
  (interactive)
  (if god-local-mode
      (god-mode-self-insert)
    (if (not (haskell-session-maybe))
        (self-insert-command 1)
      (cond ((save-excursion (forward-word -1)
                             (looking-at "^import$"))
             (insert " ")
             (let ((module (ido-completing-read "Module: " (haskell-session-all-modules))))
               (insert module)
               (haskell-mode-format-imports)))
            ((not (string= "" (save-excursion (forward-char -1) (haskell-ident-at-point))))
             (let ((ident (save-excursion (forward-char -1) (haskell-ident-at-point))))
               (shm/space)
               (haskell-process-do-try-info ident)))
            (t (shm/space))))))


;; Mode settings

(custom-set-variables
 '(haskell-process-type 'ghci)
 '(haskell-process-args-ghci '())
 '(haskell-notify-p t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t))


;; Add hook

(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)


;; Keybindings

(define-key haskell-mode-map (kbd "-") 'smart-hyphen)
(define-key haskell-mode-map [f8] 'haskell-navigate-imports)
(define-key haskell-mode-map [f5] 'haskell-process-load-or-reload)
(define-key haskell-mode-map [f12] 'turbo-devel-reload)

(define-key html-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key html-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key html-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

(define-key css-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key css-mode-map (kbd "C-`") 'haskell-interactive-bring)

(define-key haskell-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
(define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "TAB") 'haskell-simple-indent)
(define-key haskell-mode-map (kbd "<backtab>") 'haskell-simple-indent-backtab)
(define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
(define-key haskell-mode-map (kbd "C-<right>") 'haskell-move-right)
(define-key haskell-mode-map (kbd "C-<left>") 'haskell-move-left)
(define-key haskell-mode-map (kbd "<space>") 'haskell-mode-contextual-space)

(define-key haskell-interactive-mode-map (kbd "C-c C-v") 'haskell-interactive-toggle-print-mode)

(define-key haskell-cabal-mode-map [f9] 'haskell-interactive-mode-visit-error)
(define-key haskell-cabal-mode-map [f11] 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key haskell-interactive-mode-map [f9] 'haskell-interactive-mode-visit-error)
(define-key haskell-interactive-mode-map [f11] 'haskell-process-cabal-build)
(define-key haskell-interactive-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key haskell-interactive-mode-map (kbd "C-<left>") 'haskell-interactive-mode-error-backward)
(define-key haskell-interactive-mode-map (kbd "C-<right>") 'haskell-interactive-mode-error-forward)
(define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key shm-map (kbd "SPC") 'shm-contextual-space)
