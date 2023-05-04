(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(defun paredit-delete-indentation ()
  "Delete indentation and re-indent."
  (interactive)
  (delete-indentation)
  (paredit-reindent-defun))

(defun paredit-backward-delete. ()
  "A less enraging `paredit-backward-delete'."
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (call-interactively 'paredit-backward-delete)))

(defun paredit-return-or-backslash ()
  "Return to previous point in god-mode."
  (interactive)
  (if god-local-mode
      (call-interactively 'god-mode-self-insert)
    (call-interactively 'paredit-backslash)))

(defun paredit-kill-sexp ()
  "Kill the sexp at point."
  (interactive)
  (if (eq last-command 'kill-region)
      (call-interactively 'kill-sexp)
    (cond
     ((paredit-in-string-p)
      (paredit-backward-up)
      (call-interactively 'kill-sexp))
     ((paredit-inside-sexp-p)
      (paredit-backward)
      (call-interactively 'kill-sexp))
     ((paredit-start-of-sexp-p)
      (call-interactively 'kill-sexp))
     (t
      (paredit-backward)
      (call-interactively 'kill-sexp)))))

(defun paredit-delete-sexp ()
  "Delete the sexp at point."
  (interactive)
  (cond
   ((paredit-in-comment-p)
    (call-interactively 'delete-char))
   ;; Strings don't behave the same as normal sexps in paredit.
   ((paredit-in-string-p)
    (delete-region (save-excursion (paredit-backward-up)
                                   (point))
                   (save-excursion (paredit-backward-up)
                                   (paredit-forward)
                                   (point))))
   ((paredit-inside-sexp-p)
    (delete-region (save-excursion (paredit-backward)
                                   (point))
                   (save-excursion (paredit-forward)
                                   (point))))
   ((paredit-start-of-sexp-p)
    (delete-region (point)
                   (save-excursion (paredit-forward)
                                   (point))))
   ;; Otherwise we're at the end of a sexp.
   (t
    (delete-region (save-excursion (paredit-backward)
                                   (point))
                   (save-excursion (paredit-backward)
                                   (paredit-forward)
                                   (point))))))

(defun paredit-inside-sexp-p ()
  "Are we inside the bounds of a sexp?"
  (= (save-excursion (paredit-forward)
                     (point))
     (save-excursion (paredit-backward)
                     (paredit-forward)
                     (point))))

(defun paredit-start-of-sexp-p ()
  "Are we at the start of a sexp?"
  (= (save-excursion (paredit-forward)
                     (paredit-backward)
                     (point))
     (point)))
