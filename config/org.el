(defun org-fast-task-reclock (&optional no-clock)
  "Quickly make a new task and clock into it."
  (interactive "P")
  (let ((file (ido-completing-read "Org-file: " (reverse org-agenda-files))))
    (find-file file)
    (let ((headings (list)))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "^\\*" nil t)
          (destructuring-bind (_ _ status _ title _) (org-heading-components)
            (when (not status)
              (push title headings))
            (end-of-line))))
      (let ((heading (ido-completing-read "Project: " headings))
            (found nil))
        (goto-char (point-min))
        (while (and (not found)
                    (search-forward-regexp "^\\*" nil t))
          (destructuring-bind (_ _ status _ title _) (org-heading-components)
            (when (string= heading title)
              (setq found t)
              (org-insert-todo-heading-respect-content)
              (org-metaright 1)
              (org-todo "TODO")
              (insert (read-from-minibuffer "Task title: "))
              (unless no-clock (org-clock-in)))
            (end-of-line)))))))

(define-key org-mode-map (kbd "C-#") 'org-begin-template)
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "\n#+END_" choice)
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "\n#+END_" choice))))))))))

(define-key org-mode-map (kbd "C-c C-e") 'org-focus-estimate)
(define-key org-mode-map (kbd "C-c C-s") 'org-focus-schedule)

(define-key org-mode-map (kbd "C-c C-x C-i") 'org-multiclock-in)
(define-key org-mode-map (kbd "C-c C-x C-o") 'org-multiclock-out)

(setq org-clock-clocked-in-display nil)

(defun org-link ()
  (interactive)
  (execute-kbd-macro
   [?  ?s ?/ ?b ?l ?o ?b ?/ return ?g ?f ?f backspace escape ?\[ ?f ?i ?l ?e ?: escape ?s ?# return escape backspace ?: ?: escape ?d ?g ?f escape ?\] escape ?r ?\[ return]))
