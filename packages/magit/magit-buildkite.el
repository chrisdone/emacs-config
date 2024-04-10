(define-minor-mode magit-buildkite-mode
  "Add a new section to the magit status screen which loads
  Buildkite build information for the current branch."
  :init-value nil
  :lighter " BK"
  (when (eq major-mode 'magit-status-mode)
    (if magit-buildkite-mode
        (magit-add-section-hook 'magit-status-sections-hook
                                #'magit-buildkite-insert-recent-builds
                                'magit-insert-status-tags-line
                                )
      (setq magit-status-sections-hook
            (remove #'magit-buildkite-insert-recent-builds magit-status-sections-hook)))))

(defun magit-buildkite-insert-recent-builds ()
  (let ((branch (magit-get-current-branch)))
    (when branch
      (magit-with-section (section processbuf nil nil t)
        (magit-buildkite-summary)))))

(defun magit-buildkite-summary ()
  (let* ((branch-name (car (my-lines (shell-command-to-string "git symbolic-ref --short HEAD"))))
         (file (format "%s.git/bk-%s.json" (magit-get-top-dir) (sha1 branch-name))))
    (if (file-exists-p file)
        (let ((builds (json-parse-string
                       (with-current-buffer (find-file-noselect file t t nil)
                         (buffer-string))
                       :object-type 'plist)))
          (if (= 0 (length builds))
              "CI: No active builds."
            (mapconcat
             (lambda (build)
               (format "CI: %s (%s) %s %s"
                       (plist-get build :number)
                       (plist-get build :state)
                       (propertize (substring (plist-get build :commit) 0 7)
                                   'face 'magit-log-sha1)
                       (plist-get build :message)))
             builds
             "\n")))
      "CI: No data. Hit G to reload.")))

(remove-hook 'magit-refresh-all-hooks 'magit-refresh-all-buffers-function)
(add-hook 'magit-refresh-all-hooks 'magit-buildkite-refresh)
(defun magit-buildkite-refresh ()
  (interactive)
  (cond
   ((getenv "BUILDKITE_API_KEY")
    (message "Refreshing Buildkite ...")
    (let ((api-key (getenv "BUILDKITE_API_KEY"))
          (branch-name (car (my-lines (shell-command-to-string "git symbolic-ref --short HEAD")))))
      (with-output-to-string
        (with-current-buffer standard-output
          (let ((args (list "curl" nil t nil
                            "-o" (format "%s.git/bk-%s.json" (magit-get-top-dir) (sha1 branch-name))
                            (format
                             "https://api.buildkite.com/v2/organizations/%s/builds?branch=%s&per_page=1"
                             (car (split-string (cadr (git-link--parse-remote (git-link--remote-url (git-link--select-remote)))) "/"))
                             (url-hexify-string branch-name))
                            "-H" (format "Authorization: Bearer %s" api-key))))
            (apply #'call-process args)))))
    (message "Refreshed Buildkite."))
   (t (message "BUILDKITE_API_KEY not defined. Skipping."))))
