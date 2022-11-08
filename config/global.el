
;; Requirements

(require 'uniquify)
(require 'sgml-mode)
(require 'js)
(require 'org-agenda)
(require 'ielm)


;; Fundamental functions

(defun replace-not-regexp (beg end)
  "Replace the content before and after every occurence of the
prompted regexp with the prompted text."
  (interactive "r")
  (let* ((r (read-from-minibuffer "Replace not this regexp: "))
         (with (read-from-minibuffer "Replace with: "))
         (with-length (length with))
         (base beg))
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp r end t 1)
        (let ((new-base (point))
              (end (match-beginning 0)))
          (delete-region base end)
          (insert with)
          (setq base (+ new-base (- base end) with-length))))
      (when (< base end)
        (delete-region base end)
        (insert with)))))

(defun delete-blank-lines-in (start end)
  "Delete blank lines at point or in the region."
  (interactive "r")
  (replace-regexp "[\n]+" "\n" nil start end))

(defun json-pretty-print-buffer ()
  (json-reformat-region (point-min) (point-max)))

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

;; Use convert -delay 35 -colorspace rgb -colorspace srgb -quantize lab -fuzz 3%  -layers optimize *.png out.gif
;; how to get window id:
(defun snap! ()
  (interactive)
  (let ((name (format-time-string "%Y-%m-%d-%H-%M-%S-%3N")))
    (redisplay t)
    (shell-command-to-string
     (format "screencapture -l0x85FC -o /Users/chris/Emacs/packages/intero/tmp/%s.png" name))))
(global-set-key [f9] 'snap!)

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
  "Re-open current file with sudo."
  (interactive)
  (set-visited-file-name (concat "/sudo::" (buffer-file-name)))
  (when buffer-read-only
    (read-only-mode 0)))

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
  (let* ((default (if (string-match "Currently clocked IN"
                                    (shell-command-to-string "clockin status"))
                      "clockout"
                    "clockin"))
         (input
          (read-from-minibuffer (format "Track [default: %s]: " default)))
         (msg (if (string= "" input)
                  default
                input)))
    (with-current-buffer (find-file-noselect "~/Org/me.track")
      (track-mode-new-entry msg)
      (save-buffer))
    (when (or (string= msg "clockin")
              (string= msg "clockout"))
      (shell-command-to-string "clockin toggle"))))

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

(defun insert-date ()
  (interactive)
  (insert (shell-command-to-string "date +'%Y-%m-%d'")))

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

(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (let ((col (current-column)))
           (when p
             (goto-char (line-beginning-position)))
           (call-interactively ',function)
           (when p
             (forward-char col))))
       ',name)))

(defun ielm-clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ielm-return)))

(defun replace-string-or-query-replace (n)
  (interactive "P")
  (if n
      (call-interactively 'query-replace)
    (call-interactively 'replace-string)))

(defun my-section-string ()
  (cond
   ((eq major-mode 'haskell-mode)
    "^--------------------------------------------------------------------------------
")))

(defun my-forward-page ()
  "Go to next page."
  (interactive)
  (unless (= (point) (point-max)) (forward-char 1))
  (or (and (search-forward-regexp (my-section-string)
                                  nil
                                  t
                                  1)
           (my-backward-page))
      (end-of-buffer)))

(defun my-backward-page ()
  "Go to next page."
  (interactive)
  (or (search-backward-regexp (my-section-string)
                              nil
                              t
                              1)
      (beginning-of-buffer)))


;; Global keybindings

(global-set-key (kbd "<prior>") 'my-backward-page)
(global-set-key (kbd "<next>") 'my-forward-page)

(global-set-key (kbd "C-p") 'avoid-this-key)
(global-set-key (kbd "C-h") 'previous-line)

(global-set-key (kbd "C-,") 'quickjump-forward)
(global-set-key (kbd "C-m") 'quickjump-back)
(global-set-key (kbd "RET") 'newline)
(define-key input-decode-map "\C-m" [C-m])

(defun avoid-this-key ()
  (interactive)
  (error "Use `h'"))

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap shm/kill-line] (bol-with-prefix shm/kill-line))

(global-set-key (kbd "s-d") 'delete-this-line)

;; you have to do it this way, if you just global-set-key then other
;; modes rebind C-g. So s-g is much better.
(define-key key-translation-map (kbd "s-g") (kbd "C-g"))

(defun delete-this-line ()
  (interactive)
  (let ((col (current-column)))
    (delete-region (1- (line-beginning-position))
                   (line-end-position))
    (forward-line 1)
    (goto-char (+ (line-beginning-position) col))))

(global-set-key (kbd "C-x C-(") 'resmacro-start-macro)
(global-set-key (kbd "C-x C-)") 'kmacro-end-or-call-macro)

(global-set-key (kbd "M-u") 'upcase-word-and-backwards)

(global-set-key (kbd "C-c C-+") 'number/add)
(global-set-key (kbd "C-c C--") 'number/sub)
(global-set-key (kbd "C-c C-*") 'number/multiply)
(global-set-key (kbd "C-c C-/") 'number/divide)
(global-set-key (kbd "C-c C-0") 'number/pad)
(global-set-key (kbd "C-c C-=") 'number/eval)
(global-set-key (kbd "C-c C-:") 'eval-replacing-region)

(global-set-key (kbd "C-x C-k C-o") 'delete-blank-lines-in)

(global-set-key (kbd "C-\\") 'goto-last-point)
(global-set-key (kbd "M-z") 'zap-up-to-char-repeatable)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-x") 'goto-last-change)
(global-set-key (kbd "C-x C-TAB") 'indent-rigidly)
(global-set-key (kbd "C-t") 'replace-string-or-query-replace)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-x l") 'select-current-line)
(global-set-key (kbd "M-a") 'backward-up-list)
(global-set-key (kbd "M-a") 'up-list)

(add-hook 'compilation-mode-hook 'tail-on-change-mode)

(define-key global-map [?\M-1] '(lambda () (interactive) (tab-bar-select-tab 1)))
(define-key global-map [?\M-2] '(lambda () (interactive) (tab-bar-select-tab 2)))
(define-key global-map [?\M-3 ] '(lambda () (interactive) (tab-bar-select-tab 3)))
(define-key global-map [?\M-4] '(lambda () (interactive) (tab-bar-select-tab 4)))
(define-key global-map [?\M-5] '(lambda () (interactive) (tab-bar-select-tab 5)))
(define-key global-map [?\M-6] '(lambda () (interactive) (tab-bar-select-tab 6)))
(define-key global-map [?\M-7] '(lambda () (interactive) (tab-bar-select-tab 7)))
(define-key global-map [?\M-8] '(lambda () (interactive) (tab-bar-select-tab 8)))
(define-key global-map [?\M-9] '(lambda () (interactive) (tab-bar-select-tab 9)))

(define-key magit-mode-map [?\M-1] '(lambda () (interactive) (tab-bar-select-tab 1)))
(define-key magit-mode-map [?\M-2] '(lambda () (interactive) (tab-bar-select-tab 2)))
(define-key magit-mode-map [?\M-3 ] '(lambda () (interactive) (tab-bar-select-tab 3)))
(define-key magit-mode-map [?\M-4] '(lambda () (interactive) (tab-bar-select-tab 4)))
(define-key magit-mode-map [?\M-5] '(lambda () (interactive) (tab-bar-select-tab 5)))
(define-key magit-mode-map [?\M-6] '(lambda () (interactive) (tab-bar-select-tab 6)))
(define-key magit-mode-map [?\M-7] '(lambda () (interactive) (tab-bar-select-tab 7)))
(define-key magit-mode-map [?\M-8] '(lambda () (interactive) (tab-bar-select-tab 8)))
(define-key magit-mode-map [?\M-9] '(lambda () (interactive) (tab-bar-select-tab 9)))

(global-set-key (kbd "C-x C-z") 'dogears-list)
(global-set-key (kbd "C-z") 'ido-switch-buffer)

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

(global-set-key [f9] nil)
(global-set-key [f10] nil)
(global-set-key [f11] nil)
(global-set-key [f12] nil)
(global-set-key [f3] nil)
(global-set-key (kbd "C-x (") nil)
(global-set-key (kbd "C-x C-c") nil)

;; Convenience bindings for god-mode

(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-!") 'eval-defun)




;; Mode-specific keybindings

(define-key inferior-emacs-lisp-mode-map (kbd "C-c C-k") 'ielm-clear)
(define-key shell-mode-map (kbd "C-c C-k") 'erase-buffer)
(define-key org-mode-map (kbd "C-,") nil)
(define-key js-mode-map (kbd "C-c C-l") 'javascript-console-log)
(define-key sgml-mode-map (kbd "/") nil)
(define-key c-mode-map (kbd "/") nil)

(defun my-comint-send ()
  "Less silly return key for comint-mode."
  (interactive)
  (if (comint-after-pmark-p)
      (call-interactively 'comint-send-input)
    (goto-char (point-max))))

(defun my-comint-prev ()
  "Less silly return key for comint-mode."
  (interactive)
  (if (comint-after-pmark-p)
      (call-interactively 'comint-previous-input)
    (progn (goto-char (point-max))
           (call-interactively 'comint-previous-input))))

(define-key comint-mode-map (kbd "RET") 'my-comint-send)
(define-key comint-mode-map (kbd "M-p") 'my-comint-prev)

(define-key messages-buffer-mode-map (kbd "C-c C-k") 'messages-erase-buffer)
(defun messages-erase-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(define-key ag-mode-map (kbd "p") 'previous-error-no-select)
(define-key ag-mode-map (kbd "n") 'next-error-no-select)


;; Disable default settings

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default dired-listing-switches "-alh")
(setq dired-listing-switches "-alh")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq kmacro-execute-before-append nil)

(set-default 'tags-case-fold-search nil)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Enable cool modes
(ido-mode 1)
(global-font-lock-mode 1)

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "\\.dyn_hi$\\|\\.dyn_o$\\|\\.hi$\\|\\.o$")


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
(blink-cursor-mode -1)

(setq gnus-button-url 'browse-url-generic)

(setq ido-ignore-files '("\\.dyn_hi$""\\.dyn_o$""\\.hi$" "\\.o$" "\\.tags$" "^\\.ghci$"))
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
      org-priority-faces (quote ((49 . zenburn-red)
                                 (50 . zenburn-yellow)
                                 (51 . zenburn-green))))

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "STARTED(s)"
         "BLOCKED(b@/!)"
         "CONFIRM(w!)"
         "|"
         "DONE(d!)"
         "CANCELED(c@)"
         "DEFERRED(c@)")))

(setq org-priority-faces (quote ((49 . sunburn-red)
                                 (50 . sunburn-yellow)
                                 (51 . sunburn-green))))
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

(add-to-list 'auto-mode-alist (cons "\\.track\\'" 'track-mode))

(add-to-list 'auto-mode-alist (cons "\\.tsx\\'" 'web-mode))
(add-to-list 'auto-mode-alist (cons "\\.ts\\'" 'typescript-mode))


;; Environment settings

(set-language-environment "UTF-8")


;; Faces

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
 '(magit-status-buffer-switch-function 'switch-to-buffer)
 '(safe-local-variable-values
   (quote ((haskell-indent-spaces . 4)
           (haskell-process-use-ghci . 4)
           (haskell-indent-spaces . 2)
           (haskell-process-type . cabal-repl)
           (shm-lambda-indent-style . leftmost-parent)))))

(defmacro measure-time (label body)
  "Measure the time it takes to evaluate BODY."
  (let ((sym (gensym "time"))
        (result (gensym "result")))
    `(let ((,sym (current-time))
           (,result ,body))
       (message "%s: %.0fms"
                ,label
                (* 1000 (float-time (time-since ,sym))))
       ,result)))

(setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
                                        ;(setq flycheck-check-syntax-automatically '(save mode-enabled))

(remove-hook 'post-command-hook 'slowdown)
(defun slowdown ()
  (redisplay)
  (sleep-for 0.25))

(setq comint-input-ignoredups t)

(defun increment-character ()
  (interactive)
  (save-excursion
    (unless (looking-at "[a-zA-Z]")
      (when (looking-back "[a-zA-Z]")
        (forward-char -1)))
    (let ((c (string-to-char (buffer-substring-no-properties (point) (+ 1 (point))))))
      (delete-region (point) (+ 1 (point)))
      (insert (char-to-string (+ 1 c))))))
(global-set-key (kbd "C-x C-c") 'increment-character)

(defun wget (url)
  (interactive "sEnter URL: ")
  (let ((filename (read-from-minibuffer "Filename: " (car (last (split-string url "/" t))))))
    (url-copy-file url filename t)
    (find-file filename)))

(defun curl (url)
  (interactive "sEnter URL: ")
  (url-insert-file-contents url nil nil nil t))

(global-set-key (kbd "C-=") 'resmacro-start-macro)
(global-set-key (kbd "C-!") 'kmacro-end-or-call-macro)

(global-set-key (kbd "<down-mouse-1>") 'mouse-drag-region)
(global-set-key (kbd "<mouse-1>") 'my-down-mouse)

(defun my-down-mouse ()
  (interactive)
  (let ((overlay-buttons (overlays-in (point) (1+ (point))))
        (called nil))
    (mapc (lambda (o)
            (when (overlay-get o 'button)
              (when (overlay-get o 'action)
                (funcall (overlay-get o 'action) o)
                (setq called t))))
          overlay-buttons)
    (unless called
      (let ((button (get-text-property (point) 'button)))
        (when (get-text-property (point) 'action)
          (funcall (get-text-property (point) 'action)
                   (point)))))))

(setq audit-file-pattern "^src/.*?\\.hs$")

(defun changelog-new ()
  (interactive)
  (execute-kbd-macro [?g ?< return return ?  ?e ?g ?w ?h ?h ?y ?g ?b ?  ?g ?f ?c ?+ ?1 return ?e return escape ?* ?\S-  escape]))

(define-key audit-mode-map (kbd "C-' C-'") 'audit-comment)
(define-key audit-mode-map (kbd "C-' C-;") 'audit-ok)

(setq remote-file-name-inhibit-cache t)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose -1)
(global-eldoc-mode -1)
(setq tramp-copy-size-limit 100000)
(setq tramp-inline-compress-start-size 100000)
(setq vc-handled-backends ())
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'find-file-hook 'vc-refresh-state)

(defun ushell ()
  (interactive)
  (let ((proj (ido-completing-read
               "Repo: "
               (split-string
                (replace-regexp-in-string
                 "\n$" ""
                 (shell-command-to-string "ls -1 ~/Work/ | for i in $(cat /dev/stdin); do for j in $(ls -1 ~/Work/$i); do echo $i/$j; done; done"))
                "\n"))))
    (let ((default-directory (concat "~/Work/" proj)))
      (shell (concat "*sh:" proj "*")))))

(defun wait ()
  (interactive)
  (message "Waiting for 1 second ...")
  (redisplay)
  (sleep-for 1)
  (message "Done waiting."))

(setq rust-format-on-save t)
(setq rust-rustfmt-bin "/home/chris/.cargo/bin/rustfmt")

(custom-set-variables '(git-link-use-commit t))

(defun snake-case ()
  "Make the symbol at point snake_case."
  (interactive)
  (let* ((point (point))
         (points (bounds-of-thing-at-point 'symbol))
         (string (buffer-substring (car points) (cdr points))))
    (save-excursion
      (goto-char (car points))
      (delete-region (car points) (cdr points))
      (insert (string-inflection-underscore-function string)))
    (goto-char point)))

(defun camel-case ()
  "Make the symbol at point camelCase."
  (interactive)
  (let* ((point (point))
         (points (bounds-of-thing-at-point 'symbol))
         (string (buffer-substring (car points) (cdr points))))
    (save-excursion
      (goto-char (car points))
      (delete-region (car points) (cdr points))
      (insert (string-inflection-camelcase-function string)))
    (goto-char point)))

(defun pascal-case ()
  "Make the symbol at point PascalCase."
  (interactive)
  (let* ((point (point))
         (points (bounds-of-thing-at-point 'symbol))
         (string (buffer-substring (car points) (cdr points))))
    (save-excursion
      (goto-char (car points))
      (delete-region (car points) (cdr points))
      (insert (string-inflection-pascal-case-function string)))
    (goto-char point)))

(defun kebab-case ()
  "Make the symbol at point kebab-case."
  (interactive)
  (let* ((point (point))
         (points (bounds-of-thing-at-point 'symbol))
         (string (buffer-substring (car points) (cdr points))))
    (save-excursion
      (goto-char (car points))
      (delete-region (car points) (cdr points))
      (insert (string-inflection-kebab-case-function string)))
    (goto-char point)))

(setq suggest-key-bindings nil)
(put 'narrow-to-region 'disabled nil)
(setq ido-auto-merge-delay-time 99999)
(setq org-time-stamp-rounding-minutes '(0 15))
(setq-default ediff-forward-word-function 'forward-char)

(global-set-key (kbd "C-x C-\\") 'winner-undo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smerge

(defun my-enable-smerge-maybe ()
  (when buffer-file-name
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^<<<<<<< " nil t)
          (unless smerge-mode
            (smerge-mode +1))
        (when smerge-mode
          (smerge-mode -1))))))

;; Enable when needed
;; (add-hook 'buffer-list-update-hook 'my-enable-smerge-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicate

(defun yank-replacing ()
  (interactive)
  (let* ((rep (read-from-minibuffer "Replace: "))
         (with (read-from-minibuffer "With: "))
         (original (current-kill 0))
         (replacement (replace-in-string original rep with)))
    (insert replacement)))

(defun duplicate (beg end)
  (interactive "r")
  (let* ((rep (read-from-minibuffer "Replace: "))
         (with (read-from-minibuffer "With: "))
         (original (buffer-substring beg end))
         (replacement (replace-in-string original rep with)))
    (kill-new replacement)))
(global-set-key (kbd "M-W") 'duplicate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

;; (flycheck-define-generic-checker 'todo
;;   "todos"
;;   :start 'todo-checker
;;   :modes '(haskell-mode literate-haskell-mode javascript-mode yaml-mode))

;; (defun todo-checker (checker cont)
;;   )

;; (add-to-list 'flycheck-checkers 'intero)

(defun copy-file-name ()
  "Copy the buffer's file name."
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied %s" (buffer-file-name)))

(defun copy-buffer-name ()
  "Copy the buffer's buffer name."
  (interactive)
  (kill-new (buffer-name))
  (message "Copied %s" (buffer-name)))

(defun fork-elscreen ()
  "Fork the current elscreen with a generated name of `NAME*', makes it easy to see nesting."
  (interactive)
  (let ((name (concat (elscreen-get-screen-nickname (elscreen-get-current-screen))
                      "-"
                      (format "%S" (float-time)))))
    (setf elscreen-fork-n (+ elscreen-fork-n 1))
    (elscreen-clone)
    (elscreen-screen-nickname name)))

(defun graphviz-ascii-on-region (beg end)
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (with-current-buffer (get-buffer-create "*graphviz-ascii*")
      (delete-region (point-min) (point-max)))
    (call-process-region beg end "graph-easy" nil (get-buffer "*graphviz-ascii*") t
                         "--from=dot"
                         "--as_ascii")
    (display-buffer
     (get-buffer "*graphviz-ascii*"))))

(defun graphviz-ascii ()
  "Generates ascii on the buffer."
  (interactive)
  (graphviz-ascii-on-region (point-min) (point-max)))

(define-key graphviz-dot-mode-map (kbd "C-M-x") 'graphviz-ascii)

(setq tab-bar-close-button "")
(setq tab-bar-new-button "")

(defun sql-pretty-print (beg end)
  (interactive "r")
  (call-process-region beg end "sqlformat" t t t "-r" "/dev/stdin"))

(define-key compilation-mode-map (kbd "C-v C-n") 'my-compilation-next-error)
(define-key compilation-mode-map (kbd "C-v C-h") 'my-compilation-previous-error)

(defun my-compilation-next-error ()
  (interactive)
  (compilation-next-error 1)
  (call-interactively 'compile-goto-error nil))

(defun my-compilation-previous-error ()
  (interactive)
  (compilation-previous-error 1)
  (call-interactively 'compile-goto-error nil))

(defun ps-foreign-reload ()
  (interactive)
  (let ((name (buffer-name)))
    (let ((match (string-match "\\(.*\\)\\.js" name)))
      (when match
        (let ((purs-name (concat (match-string 1 name) ".purs")))
          (let ((buffer (get-buffer purs-name)))
            (when buffer
              (message "Re-type-checking %s" purs-name)
              (with-current-buffer purs-name
                (flycheck-buffer)))))))))

(defun my-after-save-js-action ()
  "Used in `after-save-hook'."
  (when (memq this-command '(save-buffer save-some-buffers))
    (when (eq  major-mode 'js-mode)
      (ps-foreign-reload))))

(add-hook 'after-save-hook 'my-after-save-js-action)

(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

(setq web-mode-enable-auto-indentation nil)

(defun unjson-string (beg end)
  "The region containing a JSON string will be unJSON-ified."
  (interactive "r")
  (let* ((string (buffer-substring beg end))
         (input (if (string-match "^\".*\"$" string)
                    string
                  (format "\"%s\"" string)))
         (output (with-temp-buffer
                   (insert input)
                   (call-process-region
                    (point-min)
                    (point-max)
                    "jq"
                    t
                    t
                    nil
                    "--raw-output")
                   (buffer-string))))
    (delete-region beg end)
    (insert output)))

(provide 'global)
