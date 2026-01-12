(define-derived-mode note-mode
  text-mode "Note"
  "Major mode for writing notes.
 \\{note-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(note-keywords t nil nil))
  (jumpto-address-mode 1))

(add-to-list 'auto-mode-alist '("\\.note\\'" . note-mode))

(defun note-stash ()
  (interactive)
  (let* ((content (if (region-active-p)
                      (buffer-substring (region-beginning) (region-end))
                    ""))
         (id (note-generate-nanoid))
         (fp (concat id ".note")))
    (unless (string= "" content)
      (delete-region (region-beginning) (region-end)))
    (insert (format "[%s](%s)" id fp))
    (if (string= "" content)
        (switch-to-buffer-other-window (find-file-noselect fp))
      (with-current-buffer (find-file-noselect fp)
        (insert content)
        (save-buffer)
        (kill-buffer)))))

(defun note-generate-nanoid ()
  "Generate a Nano ID of the form `note_NGMyMDVkZjZiYTVlZTVhM' using SHA-1."
  (let* ((random-string (format "%s%s%S" (emacs-pid) (current-time-string) (random)))
         (sha1-hash (secure-hash 'sha1 random-string))
         (base64-encoded (base64-encode-string sha1-hash))
         (nanoid (string-trim-right (substring base64-encoded 0 21))))
    (concat "note_" nanoid)))

(define-key note-mode-map (kbd "C-@") 'note-stash)
