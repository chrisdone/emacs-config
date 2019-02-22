(defun lpaste-region (beg end)
  "Paste the region to lpaste.net."
  (interactive "r")
  (let ((response
         (shell-command-to-string
          (format "curl -D/dev/stdout \"http://lpaste.net/new?%s\""
                  (mapconcat 'identity
                             (mapcar (lambda (cons)
                                       (concat (url-hexify-string (car cons))
                                               "="
                                               (url-hexify-string (cdr cons))))
                                     `(("title" . ,(read-from-minibuffer "Title: "))
                                       ("author" . ,lpaste-author)
                                       ("language" . ,(cond ((eq major-mode 'haskell-mode)
                                                             "haskell")
                                                            ((eq major-mode 'emacs-lisp-mode)
                                                             "elisp")
                                                            (t
                                                             "")))
                                       ("channel" . "")
                                       ("paste" . ,(buffer-substring-no-properties beg end))
                                       ("private" . "private")
                                       ("email" . "")))
                             "&")))))
    (when (string-match "Location: /\\([0-9]+\\)" response)
      (message "%S" (match-string 1 response))
      (browse-url (concat "http://lpaste.net/"
                          (match-string 1 response))))))

(provide 'lpaste)
