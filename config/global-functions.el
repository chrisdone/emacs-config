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
  (linum-mode -1))

(defun ls-files ()
  (interactive)
  (let ((default-directory (car (split-string (shell-command-to-string "git rev-parse --show-toplevel")))))
    (find-file
     (ivy-completing-read
      "Find File: "
      (split-string (shell-command-to-string "git ls-files") "\n")))))

(defun rg ()
  (interactive)
  (counsel-rg (ag/dwim-at-point)))
