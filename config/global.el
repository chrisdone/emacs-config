
;; Requirements

(require 'uniquify)
(require 'sgml-mode)
(require 'js)


;; Fundamental functions

(defun eval-replacing-region (read)
  "Eval an expression on the region and replace the region with the
  result."
  (interactive "P")
  (unless (region-active-p)
    (error "Region is not active!"))
  (let* ((string
          (buffer-substring-no-properties
           (region-beginning)
           (region-end)))
         (function
          (eval
           `(lambda (x)
              ,(read-from-minibuffer "Expression on x: " "" nil t))))
         (result (funcall function (if read (read string) string)))
         (start (point)))
    (delete-region (region-beginning)
                   (region-end))
    (insert (case (type-of result)
              (string (format "%s" result))
              (t (format "%S" result))))
    (set-mark (point))
    (goto-char start)))

(defun smart-hyphen (n)
  "Capitalize the next typed letter, or behave as the usual '-'."
  (interactive "p")
  (if (memq (get-text-property (point) 'face)
            '(font-lock-doc-face
              font-lock-comment-face
              font-lock-string-face))
      (self-insert-command n)
    (insert ?-)
    (let ((command (key-binding (vector (read-event)))))
      (if (eq command 'self-insert-command)
          (insert
           (let ((next (elt (this-command-keys) 1)))
             (if (eq ?w (char-syntax next))
                 (progn
                   (delete-char -1)
                   (upcase next))
               next)))
        (call-interactively command)))))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (execute-kbd-macro [?\C-e S-home]))

(defun set-auto-saves ()
  "Put autosave files (ie #foo#) in one place, *not*
 scattered all over the file system!"
  (defvar autosave-dir
    (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

  (make-directory autosave-dir t)

  (defun auto-save-file-name-p (filename)
    (string-match "^#.*#$" (file-name-nondirectory filename)))

  (defun make-auto-save-file-name ()
    (concat autosave-dir
            (if buffer-file-name
                (concat "#" (file-name-nondirectory buffer-file-name) "#")
              (expand-file-name
               (concat "#%" (buffer-name) "#")))))

  (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
  (setq backup-directory-alist (list (cons "." backup-dir))))

(defun auto-chmod ()
  "If we're in a script buffer, then chmod +x that script."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (shell-command (concat "chmod u+x " buffer-file-name))
       (message (concat "Saved as script: " buffer-file-name))))

(defun find-alternate-file-with-sudo ()
  "Re-open with sudo."
  (interactive)
  (let ((point (point)))
    (find-alternate-file (concat "/sudo::" (buffer-file-name)))
    (goto-char point)))

(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun paredit-delete-indentation ()
  "Delete indentation and re-indent."
  (interactive)
  (delete-indentation)
  (paredit-reindent-defun))

(defun github-ticket-open (&optional ticket)
  "Open the ticket number at point."
  (interactive)
  (let ((number (or ticket
                    (github-get-ticket))))
    (unless (string= number "")
      (browse-url (concat github-ticket-prefix number)))))

(defun github-get-ticket ()
  "Get the ticket number at point."
  (save-excursion
    (when (looking-at "#")
      (forward-char))
    (search-backward-regexp "[^0-9]" (line-beginning-position) t 1)
    (forward-char)
    (let* ((start (point))
           (number (progn (search-forward-regexp "[0-9]+" (line-end-position) t)
                          (buffer-substring-no-properties start
                                                          (point)))))
      number)))

(defun project-todo ()
  "Generate a TODO.org file from the project's files."
  (interactive)
  (let ((dir (or (when (boundp 'project-directory) project-directory)
                 (ido-read-directory-name "Project dir: " default-directory))))
    (find-file (concat dir "/TODO.org"))
    (erase-buffer)
    (insert (shell-command-to-string (concat "todo " dir)))
    (save-buffer)))

(defun my-yank (&optional previous-line)
  "Yank from the kill ring with some special newline behaviour."
  (interactive "P")
  (let ((string (current-kill 0)))
    (cond
     ((string-match "\n" string)
      (cond
       (previous-line
        (goto-char (line-beginning-position))
        (save-excursion
          (clipboard-yank)))
       (t
        (goto-char (line-end-position))
        (forward-char)
        (save-excursion
          (clipboard-yank)))))
     (t (clipboard-yank)))))

(defun set-ansi-colors ()
  (interactive)
  (setq ansi-color-names-vector
        (list zenburn-bg
              zenburn-red
              zenburn-green
              zenburn-yellow
              zenburn-blue
              zenburn-magenta
              zenburn-cyan
              zenburn-fg))
  (setq ansi-color-map (ansi-color-make-color-map)))

(defvar zap-up-to-char-last-char nil
  "The last char used with zap-up-to-char-repeateable.")
(defvar zap-up-to-char-last-arg 0
  "The last direction used with zap-up-to-char-repeateable.")
(defun zap-up-to-char-repeatable (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (if (and (eq last-command 'zap-up-to-char-repeatable)
                        (eq 'repeat real-this-command))
                   (list zap-up-to-char-last-arg
                         zap-up-to-char-last-char)
                 (list (prefix-numeric-value current-prefix-arg)
                       (read-char "Zap to char: " t))))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (let ((start (point))
        (end (save-excursion
               (when (eq last-command 'zap-up-to-char-repeatable)
                 (forward-char))
               (search-forward (char-to-string char) nil nil arg)
               (forward-char -1)
               (point))))
    (cond
     ((and (eq last-command 'zap-up-to-char-repeatable)
           (eq 'repeat real-this-command))
      (let ((last-command 'kill-region))
        (kill-region start end)))
     (t
      (kill-region start end))))
  (setq zap-up-to-char-last-char char)
  (setq zap-up-to-char-last-arg arg)
  (setq this-command 'zap-up-to-char-repeatable))

(defun timeclock-dwim (in)
  "Either clock in or clockout."
  (interactive "P")
  (shell-command-to-string "clockin toggle"))

(defun javascript-console-log ()
  "Insert console.log('%o',|)."
  (interactive)
  (when (not (eolp))
    (split-line))
  (insert "console.log('%o',);")
  (forward-char -2))

(defun goto-notmuch-inbox ()
  "Go to the inbox."
  (interactive)
  (notmuch-search "tag:inbox"))

(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(defun sign-comment-dwim ()
  "Add a signed comment."
  (interactive)
  (unless (or (eq 'font-lock-comment-delimiter-face
                  (get-text-property (point) 'face))
              (eq 'font-lock-doc-face
                  (get-text-property (point) 'face))
              (eq 'font-lock-comment-face
                  (get-text-property (point) 'face)))
    (insert "-- "))
  (insert sign-comment-author-name
          " ("
          (replace-regexp-in-string "\n" ""
                                    (shell-command-to-string "date +'%Y-%m-%d'"))
          "): "))

(defun reorder-buffer-list (pre-sort-list)
  "Re-order the buffer list."
  (let* ((sort-list (remove-if-not #'buffer-live-p pre-sort-list))
         (sort-len (length sort-list)))
    (mapc #'bury-buffer sort-list)
    (let* ((buffers (buffer-list))
           (buffers-len (length buffers)))
      (loop repeat (- buffers-len sort-len)
            for buf in buffers
            do (bury-buffer buf)))))

(defun save-window-config ()
  "Saves the current window configuration."
  (interactive)
  (message "Entering recursive window configuration ...")
  (let ((buffers (buffer-list)))
    (save-window-excursion
      (recursive-edit)
      (message "Restored window configuration."))
    (reorder-buffer-list buffers)))

(defun echo-mode ()
  "Start grepping /tmp/echo."
  (interactive)
  (grep "tail -f /tmp/echo -n 0"))

(defun upcase-word-and-backwards (p)
  "Upcase the following word, or on prefix arg, the previous word."
  (interactive "P")
  (if p
      (save-excursion
        (forward-word -1)
        (call-interactively 'upcase-word))
    (call-interactively 'upcase-word)))


;; Global keybindings

(global-set-key (kbd "M-u") 'upcase-word-and-backwards)

(global-set-key (kbd "C-c C-+") 'number/add)
(global-set-key (kbd "C-c C--") 'number/sub)
(global-set-key (kbd "C-c C-*") 'number/multiply)
(global-set-key (kbd "C-c C-/") 'number/divide)
(global-set-key (kbd "C-c C-0") 'number/pad)
(global-set-key (kbd "C-c C-=") 'number/eval)
(global-set-key (kbd "C-c C-:") 'eval-replacing-region)

(global-set-key (kbd "s-s") 'save-window-config)
(global-set-key (kbd "s-g") 'exit-recursive-edit)
(global-set-key (kbd "s-u") 'winner-mode-undo)

(global-set-key (kbd "C-\\") 'goto-last-point)
(global-set-key (kbd "C-v") 'magit-switch-buffer)
(global-set-key [f9] 'timeclock-dwim)
(global-set-key (kbd "M-z") 'zap-up-to-char-repeatable)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-x") 'goto-last-change)
(global-set-key (kbd "C-t") 'replace-regexp)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-x l") 'select-current-line)
(global-set-key (kbd "M-a") 'backward-up-list)
(global-set-key (kbd "M-a") 'up-list)
(global-set-key (kbd "C-z") 'ido-switch-buffer)
(global-set-key (kbd "C-c C-s") 'ace-jump-mode)

(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)

(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(global-set-key [f10] 'service/restart)
(global-set-key [f11] 'goto-notmuch-inbox)


;; Mode-specific keybindings

(define-key js-mode-map (kbd "C-c C-l") 'javascript-console-log)
(define-key paredit-mode-map (kbd "M-^") 'paredit-delete-indentation)
(define-key paredit-mode-map (kbd "M-a") 'paredit-backward-up)
(define-key markdown-mode-map (kbd "M-;") 'markdown-blockquote-region)
(define-key sgml-mode-map (kbd "/") nil)
(define-key c-mode-map (kbd "/") nil)

(define-key ag-mode-map (kbd "p") 'previous-error-no-select)
(define-key ag-mode-map (kbd "n") 'next-error-no-select)


;; Disable default settings

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(set-default 'tags-case-fold-search nil)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Enable cool modes
(ido-mode 1)
(global-font-lock-mode 1)

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files ".hi$\\|\\.o$")


;; Enable cool defaults

(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(set-auto-saves)


;; Default mode settings

(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)

(setq gnus-button-url 'browse-url-generic)

(setq ido-ignore-files '("\\.hi$" "\\.o$" "\\.tags$" "^\\.ghci$"))
(setq ido-max-directory-size 200000)

(setq browse-url-browser-function gnus-button-url)

(setq c-default-style "bsd"
      c-basic-offset 2
      c-indent-level 2)

(setq css-indent-offset 2)

(setq espresso-default-style "bsd"
      espresso-basiespresso-offset 2
      espresso-indent-level 2)

(setq org-log-done t
      org-todo-keywords '((sequence "BLOCKED" "TODO" "DONE")
                          (sequence "PASS")
                          (sequence "DEFERRED"))
      org-priority-faces (quote ((49 . zenburn-red)
                                 (50 . zenburn-yellow)
                                 (51 . zenburn-green))))
(setq org-use-fast-todo-selection t)

(setq js-indent-level 2)


;; Global settings

(setq tab-width 2)
(setq scroll-step 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq require-final-newline t)


;; Hooks

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'auto-chmod)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'shell-mode-hook 'set-ansi-colors)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)


;; Auto-loads

(add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'haskell-mode))
(add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . haskell-core-mode))

(add-to-list 'auto-mode-alist (cons "\\.lucius$" 'css-mode))
(add-to-list 'auto-mode-alist (cons "\\.julius$" 'javascript-mode))
(add-to-list 'auto-mode-alist (cons "\\.el\\'" 'emacs-lisp-mode))

(add-to-list 'auto-mode-alist (cons "\\.md\\'" 'markdown-mode))
(add-to-list 'auto-mode-alist (cons "\\.markdown\\'" 'markdown-mode))


;; Environment settings

(set-language-environment "UTF-8")


;; Faces

(custom-set-faces
 '(default ((t (:inherit nil :height 140 :width normal :family "Ubuntu Mono")))))

(defface esk-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "#bbbbbb")))
  "Face used to dim parentheses."
  :group 'starter-kit-faces)

(defface dark-paren-face
  '((((class color) (background dark))
     (:foreground "#ffffff"))
    (((class color) (background light))
     (:foreground "#000000")))
  "Face used to darken parentheses."
  :group 'starter-kit-faces)

;; Change lambda to an actual lambda symbol
(mapc (lambda (major-mode)
        (font-lock-add-keywords
         major-mode
         '(("(\\|)\\|\\[\\|\\]" . 'esk-paren-face))))
      '(emacs-lisp-mode haskell-mode))


;; Uniquify

(setq uniquify-buffer-name-style (quote post-forward-angle-brackets))


;; Safe local variables

(custom-set-variables
 '(safe-local-variable-values
   (quote ((haskell-indent-spaces . 4)
           (haskell-indent-spaces . 2)
           (haskell-process-type . cabal-repl)
           (shm-lambda-indent-style . leftmost-parent)))))
