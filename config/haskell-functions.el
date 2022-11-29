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
