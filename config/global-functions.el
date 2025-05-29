(require 'ffap)

(defun avoid-this-key ()
  (interactive)
  (error "Use `h'"))

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

(defun find-alternate-file-with-sudo ()
  "Re-open current file with sudo."
  (interactive)
  (set-visited-file-name (concat "/sudo::" (buffer-file-name)))
  (when buffer-read-only
    (read-only-mode 0)))

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

(defun delete-blank-lines-in (start end)
  "Delete blank lines at point or in the region."
  (interactive "r")
  (replace-regexp "[\n]+" "\n" nil start end))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun turn-off-linum-mode ()
  (display-line-numbers-mode -1))

(defun ls-files ()
  (interactive)
  (let ((default-directory (car (split-string (shell-command-to-string "git rev-parse --show-toplevel")))))
    (find-file
     (ivy-completing-read
      "Find File: "
      (split-string (shell-command-to-string "git ls-files") "\n")))))

(defun ls-files-dir (directory)
  (interactive (list (read-directory-name "Directory: " (car (split-string (shell-command-to-string "git rev-parse --show-toplevel"))))))
  (let ((default-directory directory))
    (find-file
     (ivy-completing-read
      "Find File: "
      (split-string (shell-command-to-string "git ls-files") "\n")))))

(defun rg ()
  (interactive)
  (counsel-rg (ag/dwim-at-point)))

(defun rg-dir (directory)
  (interactive (list (read-directory-name "Directory: ")))
  (counsel-rg (ag/dwim-at-point) directory))

(defun replace-string-or-query-replace (n)
  (interactive "P")
  (if n
      (call-interactively 'query-replace)
    (call-interactively 'replace-string)))

(defun wget (url)
  (interactive "sEnter URL: ")
  (let ((filename (read-from-minibuffer "Filename: " (car (last (split-string url "/" t))))))
    (url-copy-file url filename t)
    (find-file filename)))

(defun curl (url)
  (interactive "sEnter URL: ")
  (url-insert-file-contents url nil nil nil t))

(defun my-comint-send ()
  "Less silly return key for comint-mode."
  (interactive)
  (if (comint-after-pmark-p)
      (call-interactively 'comint-send-input)
    (let ((file-name-at-point (ffap-file-at-point)))
      (if file-name-at-point
          (let ((line (save-excursion
                        (goto-char (line-beginning-position))
                        (forward-char (length file-name-at-point))
                        (when (looking-at ":\\([0-9]+\\)")
                          (string-to-number (match-string-no-properties 1))))))
            (find-file-other-window file-name-at-point)
            (when line
              (goto-line line)
              (back-to-indentation)))
        (goto-char (point-max))))))

(defun visit-file-at-point ()
  (interactive)
  (let ((file-name-at-point (ffap-file-at-point)))
      (if file-name-at-point
          (let ((line (save-excursion
                        (goto-char (line-beginning-position))
                        (forward-char (length file-name-at-point))
                        (when (looking-at ":\\([0-9]+\\)")
                          (string-to-number (match-string-no-properties 1))))))
            (find-file-other-window file-name-at-point)
            (when line
              (goto-line line)
              (back-to-indentation)))
        (goto-char (point-max)))))

(defun my-comint-prev ()
  "Less silly return key for comint-mode."
  (interactive)
  (if (comint-after-pmark-p)
      (call-interactively 'comint-previous-input)
    (progn (goto-char (point-max))
           (call-interactively 'comint-previous-input))))

(defun treefmt ()
  (interactive)
  (shell-command "treefmt"))

(defun kmacro-! ()
  "End or call a macro."
  (interactive)
  (call-interactively 'kmacro-end-or-call-macro))

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2021  Campbell Barton
;; Author: Campbell Barton <ideasman42@gmail.com>
(defun revert-buffer-all ()
  "Refresh all open buffers from their respective files.

Buffers which no longer exist are closed.

This can be useful when updating or checking out branches outside of Emacs."
  (interactive)
  (let*
    ( ;; Pairs of '(filename . buf)'.
      (filename-and-buffer-list
        (let ((temp-list nil))
          (dolist (buf (buffer-list))
            (let ((filename (buffer-file-name buf)))
              (when filename
                (push (cons filename buf) temp-list))))
          temp-list))

      (message-prefix "Buffer Revert All:")
      (count (length filename-and-buffer-list))
      (count-final 0)
      (count-close 0)
      (count-error 0)
      ;; Keep text at a fixed width when redrawing.
      (format-count (format "%%%dd" (length (number-to-string count))))
      (format-text
        (concat message-prefix " reverting [" format-count " of " format-count "] %3d%%: %s"))
      (index 1))

    (message "%s beginning with %d buffers..." message-prefix count)
    (while filename-and-buffer-list
      (pcase-let ((`(,filename . ,buf) (pop filename-and-buffer-list)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers such as '*Messages*'.
        (message format-text index count (round (* 100 (/ (float index) count))) filename)

        (cond
          ((file-exists-p filename)
            ;; If the file exists, revert the buffer.
            (cond
              (
                (with-demoted-errors "Error: %S"
                  (with-current-buffer buf
                    (let ((no-undo (eq buffer-undo-list t)))

                      ;; Disable during revert.
                      (unless no-undo
                        (setq buffer-undo-list t)
                        (setq pending-undo-list nil))

                      (unwind-protect
                        (revert-buffer :ignore-auto :noconfirm)

                        ;; Enable again (always run).
                        (unless no-undo
                          ;; It's possible a plugin loads undo data from disk,
                          ;; check if this is still unset.
                          (when (and (eq buffer-undo-list t) (null pending-undo-list))
                            (setq buffer-undo-list nil))))))
                  t)
                (setq count-final (1+ count-final)))
              (t
                (setq count-error (1+ count-error)))))

          (t
            ;; If the file doesn't exist, kill the buffer.
            ;; No query done when killing buffer.
            (let ((kill-buffer-query-functions nil))
              (message "%s closing non-existing file buffer: %s" message-prefix buf)
              (kill-buffer buf)
              (setq count-close (1+ count-close)))))

        (setq index (1+ index))))
    (message
      (concat
        message-prefix (format " finished with %d buffer(s)" count-final)
        (cond
          ((zerop count-close)
            "")
          (t
            (format ", %d closed" count-close)))
        (cond
          ((zerop count-error)
            "")
          (t
            (format ", %d error (see message buffer)" count-error)))))))

(defun usr1-handler ()
  "Runs usr1-hooks."
   (interactive)
   (run-hooks 'usr1-hooks))

(defun async-shell-command-named (command &optional output-buffer error-buffer)
  "Copy of `async-shell-command-named' with optional naming."
  (interactive
   (list
    (read-shell-command "Named async shell command: "
                        nil nil
			(let ((filename
			       (cond
				(buffer-file-name)
				((eq major-mode 'dired-mode)
				 (dired-get-filename nil t)))))
			  (and filename (file-relative-name filename))))
    nil
    ;; FIXME: the following argument is always ignored by 'shell-commnd',
    ;; when the command is invoked asynchronously, except, perhaps, when
    ;; 'default-directory' is remote.
    shell-command-default-error-buffer))
  (let ((command-and-name (split-string command "#" nil)))
    (async-shell-command
     (nth 0 command-and-name)
     (when (nth 1 command-and-name)
       (format "*sh:%s*" (nth 1 command-and-name))))))

(defun buildkite ()
  (interactive)
  (browse-url
   (format
    "https://buildkite.com/%s/builds?branch=%s"
    (cadr (git-link--parse-remote (git-link--remote-url (git-link--select-remote))))
    (url-hexify-string (car (my-lines (shell-command-to-string "git symbolic-ref --short HEAD")))))))

(defun my-lines (s)
  (split-string (replace-regexp-in-string "[\r\n]" "\n" s) "\n" t))

(defun disable-newline-adding ()
  "Locally turn off automatically adding newlines."
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(defun sh ()
  (interactive)
  "Launch a *sh:foo* buffer."
  (let ((root (magit-get-top-dir)))
    (if root
        (let* ((name (file-name-nondirectory (directory-file-name (file-name-as-directory (magit-get-top-dir)))))
               (purpose (read-from-minibuffer "Purpose of shell: "))
               (buffer (get-buffer-create (format "*sh:%s:%s*" name purpose))))
          (shell buffer))
      (call-interactively 'shell))))

(defun strip-terminal-codes ()
  (interactive)
  (save-excursion
    (replace-regexp
     "\\(\x1B\\[[0-9;]*[A-Za-z]\\|[\x00-\x09\x0B-\x1F\x7F]\\)"
     ""
     nil
     (region-beginning)
     (region-end))))

(defun unquote-region ()
  (interactive)
  (replace-string "\\\"" "\"" nil (region-beginning) (region-end)))

(defun quote-region ()
  (interactive)
  (replace-string "\"" "\\\"" nil (region-beginning) (region-end)))

(defun tf-copy-resource-name ()
  "Copy the fully qualified name of the current resource."
  (interactive)
  (save-excursion
    (when (search-backward-regexp "^resource \"\\(.+?\\)\" \"\\(.+?\\)\"" nil t 1)
      (let* ((type (match-string 1))
             (name (match-string 2))
             (resource-name (format "%s.%s" type name)))
        (kill-new resource-name)
        (message "Copied resource name %s" resource-name)))))
