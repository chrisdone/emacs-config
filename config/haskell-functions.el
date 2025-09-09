(defun h98-reload ()
  (interactive)
  (save-buffer)
  (switch-to-buffer-other-window "*ghci*")
  (while (not (memq 'comint-highlight-prompt (get-text-property (1- (point-max)) 'face)))
    (comint-interrupt-subjob)
    (message "Waiting for GHCi job to terminate ...")
    (sit-for 0.5))
  (erase-buffer)
  (let ((last-line (ring-ref comint-input-ring 0)))
    (insert
     (if (and last-line
              (string-match "^:" last-line))
         last-line
       ":r")))
  (goto-char (point-max))
  (call-interactively 'comint-send-input))

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
  (if (executable-find "hasktags")
      (progn (if (executable-find "fd")
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
    (warn "No hasktags is installed!")))

(defun ghc-tags ()
  "Runs ghc-tags. Requires `ghc-tags' and `fd' to be installed."
  (interactive)
  (message "Running ghc-tags ...")
  (redisplay)
  (if (and (executable-find "ghc-tags")
           (executable-find "fd"))
      (let ((ghc-tags-part
             (mapconcat 'shell-quote-argument
                        (append (list "ghc-tags") (list "-e" "-o" hasktags-path))
                        " "))
            (fd-part
             (mapconcat 'shell-quote-argument (append (list "fd" "\.hs$") hasktags-directories)
                        " ")))
        (call-process "sh" nil (get-buffer-create "*ghc-tags-output*") t
                      "-c" (concat fd-part " | xargs " ghc-tags-part)))
    (warn "I need `ghc-tags' and `fd' to be installed!"))
  (message "Running ghc-tags ... done!"))

(defun fast-tags ()
  "Runs fast-tags. Requires `fast-tags' and `fd' to be installed."
  (interactive)
  (message "Running fast-tags ...")
  (redisplay)
  (if (and (executable-find "fast-tags")
           (executable-find "fd"))
      (let ((fast-tags-part
             (mapconcat 'shell-quote-argument
                        (append (list "fast-tags") (list "-e" "-o" hasktags-path))
                        " "))
            (fd-part
             (mapconcat 'shell-quote-argument (append (list "fd" "\.hs$") hasktags-directories)
                        " ")))
        (call-process "sh" nil (get-buffer-create "*fast-tags-output*") t
                      "-c" (concat fd-part " | xargs " fast-tags-part)))
    (warn "I need `fast-tags' and `fd' to be installed!"))
  (message "Running fast-tags ... done!"))

(defun haskell-refresh ()
  "Refresh databases."
  (interactive)
  (ghc-tags))

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

(defun lexx ()
  (interactive)
  (if (region-active-p)
      (shell-command-on-region
       (region-beginning)
       (region-end)
       "lexx"
       :in-place
       :replace-it)
    (shell-command-on-region
     (line-beginning-position)
     (line-end-position)
     "lexx"
     :in-place
     :replace-it)))

(defun h98-summarize-module ()
  (interactive)
  (message "%s"
           (llama-chat-on-buffer-to-string "summarize this Haskell module in one sentence")))

(defun h98-summarize-region ()
  (interactive)
  (message "%s"
           (llama-chat-on-region-to-string "summarize this Haskell code in one sentence")))

(defun my-h98-eldoc-function (callback &rest args)
  (when (eq major-mode 'h98-mode)
    (let ((start-end (hindent-decl-points)))
      (when start-end
        (lexical-let ((beg (car start-end))
                      (end (cdr start-end))
                      (callback callback))
          (llama-fold-sse-json
           :fold (lambda (acc token)
                   (cons (llama-get-token token) acc))
           :accum (list)
           :stream (make-llama-chat-stream
                    :messages
                    (vector
                     (list :role "system"
                           :content "You are an AI assistant. Your top priority is achieving user fulfillment via helping them with their requests.")
                     (list :role "user"
                           :content
                           (concat "here is a Haskell declaration\n\n"
                                   (buffer-substring-no-properties beg end)))
                     (list :role "user"
                           :content
                           (concat "give a one-sentence summary of this line of code\n\n"
                                   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
           :end (lambda (list) (funcall callback (apply 'concat (reverse list))))))))))
