(defun h98-reload ()
  (interactive)
  (save-buffer)
  (switch-to-buffer-other-window "*ghci*")
  (erase-buffer)
  (goto-char (point-max))
  (insert ":r")
  (my-comint-send))

(defun h98-recomp ()
  (interactive)
  (save-buffer)
  (switch-to-buffer-other-window "*compilation*")
  (recompile))

(defun haskell-copy-module-name ()
  "Guess the current module name of the buffer."
  (interactive)
  (kill-new (haskell-guess-module-name)))

(defun haskell-guess-module-name ()
  "Guess the current module name of the buffer."
  (interactive)
  (let ((components (cl-loop for part
                             in (reverse (split-string (buffer-file-name) "/"))
                             while (let ((case-fold-search nil))
                                     (string-match "^[A-Z]+" part))
                             collect (replace-regexp-in-string "\\.l?hs$" "" part))))
    (mapconcat 'identity (reverse components) ".")))

(defun hasktags ()
  "Runs hasktags."
  (interactive)
  (message "Running hasktags ...")
  (redisplay)
  (apply #'call-process
         (append (list "hasktags" nil (get-buffer-create "*hasktags-output*") t)
                 hasktags-directories
                 (list "-o" hasktags-path)))
  (message "Running hasktags ... done!"))

(defun hiedb-index ()
  "Runs hiedb index."
  (interactive)
  (message "Started hiedb index job.")
  (apply #'start-process
         (append (list "hiedb-index" (with-current-buffer (get-buffer-create "*hiedb-index-output*")
                                       (erase-buffer)
                                       (current-buffer))
                       "hiedb")
                 (list "index")
                 hiedb-directories
                 (list "--database" hiedb-path))))

(defun haskell-refresh ()
  "Refresh databases."
  (interactive)
  (hasktags)
  (hiedb-index))

(defun haskell-refresh-hook ()
  "Attempt to run haskell-refresh, but it's fine if it fails."
  (condition-case nil
      (haskell-refresh)
    (error (message "haskell-refresh was not successful."))))

(defun hiedb-show-type-h98 ()
  "Show type of thing at point, formatted with h98-mode."
  (interactive)
  (let ((types (hiedb-call-by-point 'hiedb-point-types)))
    (when types
      (message "%s"
               (with-temp-buffer
                 (h98-mode)
                 (insert types)
                 (font-lock-ensure)
                 (buffer-string))))))

(defun haskell-copy-imports ()
  (interactive)
  (save-excursion
    (let* ((start (progn (goto-char (point-min))
                         (search-forward-regexp "^import")
                         (line-beginning-position)))
           (end (progn (goto-char (point-max))
                       (search-backward-regexp "^import")
                       (line-end-position)))
           (string (buffer-substring start end)))
      (kill-new string)
      (message "Copied %d lines of imports." (length (split-string string "\n"))))))

(defun hiedb-goto-def-via-ivy ()
  "Jump to the definition of thing at point."
  (interactive)
  (let ((default-directory (car (split-string (shell-command-to-string "git rev-parse --show-toplevel")))))
    (let* ((locations (hiedb-call-by-point 'hiedb-point-defs))
           (location (car locations)))
      (when location
        (when (fboundp 'xref-push-marker-stack) ;; Emacs 25
          (xref-push-marker-stack))
        (find-file
         (ivy-completing-read
          "Find File: "
          (split-string (shell-command-to-string "git ls-files") "\n")
          nil nil
          (plist-get location :module)))
        (goto-char (point-min))
        (forward-line (1- (plist-get location :line)))
        (goto-char (line-beginning-position))
        (forward-char (plist-get location :column))))))

(defun hiedb-or-tags-goto-def ()
  "Go to definition via ivy, or else go via TAGS."
  (interactive)
  (condition-case nil
      (hiedb-goto-def-via-ivy)
    (error
     (call-interactively 'xref-find-definitions))))

(defun haskell-apply-suggestions ()
  "Copy suggestions from the buffer of errors/warnings and then
apply them in the current buffer."
  (interactive)
  (let* ((source-buffers
          (remove-if-not
           (lambda (buffer)
             (get-buffer-window buffer))
           haskell-suggestion-buffers))
         (suggestions
          (progn
            (when (not source-buffers)
              (error "No buffers are visible from your list: %S" haskell-suggestion-buffers))
            (with-current-buffer (car source-buffers)
              (intero-collect-compiler-messages (intero-parse-errors-warnings-splices (buffer-string))))))
         (applicable
          (remove-if-not (lambda (suggestion)
                           (string= (buffer-file-name)
                                    (plist-get (plist-get suggestion :msg) :filename)))
                         suggestions)))
    (when applicable
      (intero-apply-suggestions applicable))))

(defun haskell-show-compile-buffer ()
  (interactive)
  (switch-to-buffer-other-window "*ghci*"))
