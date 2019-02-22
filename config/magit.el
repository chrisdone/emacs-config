(require 'magit-blame)

(setq magit-blame-detect-copies-moves t)


;; Functions

(defun magit-hook-prompt-missing-ticket ()
  "Prompts to cancel the `magit-log-edit-commit' operation if no ticket reference found."
  (message "Hey!")
  (unless (save-excursion
            (goto-char (point-min))
            (search-forward-regexp "\\(refs\\|closes\\|fixes\\) #[0-9]+" nil t))
    (when (y-or-n-p "Didn't find a ticket reference. Edit commit message further? ")
      (signal 'quit nil))))

(defun magit-checkout-my-version ()
  "Checkout the last commit that I made involving this file."
  (interactive)
  (let ((hash
         (shell-command-to-string
          (concat "git log -1 --author $(git config --get user.email) --date-order --all --oneline --pretty=format:%h "
                  (buffer-file-name)))))
    (magit-show-commit hash)
    (when (y-or-n-p (format "Checkout %s? " hash))
      (magit-checkout hash))))

(defun magit-my-this-week ()
  "Show the changes I did this week."
  (interactive)
  (magit-key-mode-popup-logging "git log --oneline --since $(date --date '2 weeks ago' +'%%Y-%%m-%%d') --author $(git config --get user.email)"))

(defun ofv-magit-log-for-paths ()
  (interactive)
  (let ((paths (ed-string "Files or" "  directories: \n")))
    (apply 'magit-log nil "--" (split-string paths))))

(defun magit-switch-buffer ()
  "Interactively switch to another magit-status buffer."
  (interactive)
  (switch-to-buffer
   (format
    "*magit: %s*"
    (ido-completing-read
     "Magit buffer: "
     (loop for buffer in (buffer-list)
           as name = (buffer-name buffer)
           when (string-match "^\\*magit: \\(.+\\)\\*$" name)
           collect (match-string 1 name))))))

(defun magit-insert-clock-string ()
  (interactive)
  (insert (replace-regexp-in-string "%" "%%" org-clock-heading)))


;; Hooks

(add-hook 'magit-log-edit-commit-hook 'magit-hook-prompt-missing-ticket)

(define-key magit-log-edit-mode-map (kbd "C-c t") 'magit-insert-clock-string)

(magit-key-mode-insert-action
 'logging "p" "Paths" 'ofv-magit-log-for-paths)

(setq magit-diff-refine-hunk nil)
