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
  "Runs hasktags. If `fd' is installed and in the PATH, it'll be much faster."
  (interactive)
  (message "Running hasktags ...")
  (redisplay)
  (if (executable-find "fd")
      (let ((hasktags-part
             (mapconcat 'shell-quote-argument
                        (append (list "hasktags") (list "-o" hasktags-path))
                        " "))
            (fd-part
             (mapconcat 'shell-quote-argument (append (list "fd" "\.hs$") hasktags-directories)
                        " ")))
        (call-process "sh" nil (get-buffer-create "*hasktags-output*") t
                      "-c" (concat fd-part " | xargs " hasktags-part)))
    (apply #'call-process
           (append (list "hasktags" nil (get-buffer-create "*hasktags-output*") t)
                   hasktags-directories
                   (list "-o" hasktags-path))))
  (message "Running hasktags ... done!"))

(defun haskell-refresh ()
  "Refresh databases."
  (interactive)
  (hasktags))

(defun haskell-refresh-hook ()
  "Attempt to run haskell-refresh, but it's fine if it fails."
  (condition-case nil
      (haskell-refresh)
    (error (message "haskell-refresh was not successful."))))

(defun haskell-copy-imports ()
  (interactive)
  (save-excursion
    (let* ((start (progn (goto-char (point-min))
                         (search-forward-regexp "^import ")
                         (line-beginning-position)))
           (end (progn (goto-char (point-max))
                       (search-backward-regexp "^import ")
                       (line-end-position)))
           (string (buffer-substring start end)))
      (kill-new string)
      (message "Copied %d lines of imports." (length (split-string string "\n"))))))

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

(defun haskell-add-import (line)
  (interactive "sImport: ")
  (save-excursion
    (goto-char (point-min))
    (haskell-navigate-imports)
    (if (string-match "\n" line)
        (insert line "\n")
      (insert "import " (replace-regexp-in-string "^import " "" line) "\n"))))
