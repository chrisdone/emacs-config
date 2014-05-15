(require 'comint)
(require 'notify)

(defcustom turbo-devel-directory
  nil
  "Default directory to prompt for for turbo-devel'ing.")

(defcustom turbo-devel-command
  "turbo-devel"
  "Default command to run for turbo-devel'ing.")

(defcustom turbo-devel-port
  9091
  "Default port for listening on.")

(defun turbo-devel-mode ()
  "Trubo devel mode."
  (interactive)
  (setq major-mode 'turbo-devel-mode
        mode-name "turbo-devel"
        mode-line-process '(":%s"))
  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output
         comint-postoutput-scroll-to-bottom
         comint-truncate-buffer
         turbo-devel-filter)))

(defun turbo-devel-filter (input)
  "Picks up changes."
  (when (string-match "Devel application launched: " input)
    (message "turbo-devel: Restarted.")
    (notify "turbo-devel" "Restarted.")
    (shell-command-to-string "alert-done")))

(defun run-turbo-devel ()
  "Run the turbo devel server."
  (interactive)
  (let ((path (read-from-minibuffer "Directory: " (if turbo-devel-directory
                                                      turbo-devel-directory
                                                    default-directory)))
        (command (read-from-minibuffer "Command: " (format "%s %d"
                                                           turbo-devel-command
                                                           turbo-devel-port)))
        (buf (get-buffer-create (read-from-minibuffer "Buffer name: " "*turbo-devel*"))))
    (shell buf)
    (with-current-buffer buf
      (turbo-devel-mode)
      (insert "cd " path)
      (comint-send-input nil t)
      (insert "unset GHC_PACKAGE_PATH")
      (comint-send-input nil t)
      (insert command)
      (comint-send-input nil t)
      (erase-buffer))))

(defun turbo-devel-reload ()
  "Recompile and restart the server."
  (interactive)
  (shell-command-to-string
   (format "echo reload | nc localhost %d"
           turbo-devel-port)))

(provide 'turbo-devel-mode)
