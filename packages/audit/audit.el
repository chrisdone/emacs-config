;;; audit.el --- Audit codebases.

;; Copyright (c) 2018 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reviewing mode

(defcustom audit-file-pattern "^[^.]"
  "Include only files that match this pattern. Applies to relative paths."
  :group 'audit)

(defface audit-ok-face
  '((t :foreground "#a9b290"
       :background "#f8ffe5"
       :strike-through t
       :weight normal
       :underline nil))
  "Face for code that is OK."
  :group 'audit)

(defface audit-comment-face
  '((t :background "#fffae5"))
  "Face for code that has a comment."
  :group 'audit)

(defface audit-heading-face
  '((t :background "#fcf0bf"
       :weight bold))
  "Face for code that has a comment."
  :group 'audit)

(defvar audit-mode-map (make-sparse-keymap)
  "Keymap for reviewing mode.")

(defvar audit-cache (list)
  "Cache for all audit selections and comments.")

(defvar audit-db-path "~/.emacs.d/audit-db.el"
  "Where to write the audit database.")

(define-minor-mode audit-mode "Auditing global minor mode."
  :lighter " Audit"
  :keymap audit-mode-map
  :global t
  (if (bound-and-true-p audit-mode)
      (add-hook 'after-load-functions 'audit-refresh)
    (remove-hook 'after-load-functions 'audit-refresh)))

(defun audit-cache ()
  "Get the audit-cache, from disk if it's empty."
  (unless audit-cache (audit-load))
  audit-cache)

(defun audit-save ()
  "Save the audit database."
  (interactive)
  (with-temp-file audit-db-path
    (insert (format "%S" (audit-cache)))))

(defun audit-load ()
  "Load the audit database."
  (interactive)
  (when (file-exists-p audit-db-path)
    (setq audit-cache
          (with-temp-buffer
            (insert-file-contents audit-db-path)
            (read (current-buffer))))))

(defun audit-comment (beg end)
  "Make a comment on a region."
  (interactive "r")
  (add-to-list
   'audit-cache
   (list :file (buffer-file-name)
         :start beg
         :end end
         :commit (audit-git-commit)
         :type 'comment
         :comment (read-from-minibuffer "Comment: ")))
  (audit-save)
  (audit-refresh))

(defun audit-ok (beg end)
  "Mark a region OK."
  (interactive "r")
  (add-to-list
   'audit-cache
   (list :file (buffer-file-name)
         :start beg
         :end end
         :commit (audit-git-commit)
         :type 'ok))
  (audit-save)
  (audit-refresh))

(defun audit-delete ()
  (interactive)
  (setq audit-cache
        (let ((overlays
               (mapcar
                (lambda (o) (overlay-get o 'audit-item))
                (overlays-in (point) (1+ (point))))))
          (cl-remove-if
           (lambda (item)
             (member item overlays))
           audit-cache)))
  (audit-save)
  (audit-refresh))

(defun audit-refresh (&optional n)
  "Refresh the overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'audit-overlay t)
  (mapc (lambda (note)
          (when (string= (plist-get note :file) (buffer-file-name))
            (let ((o (make-overlay
                      (save-excursion
                        (goto-char (plist-get note :start))
                        (line-beginning-position))
                      (save-excursion
                        (goto-char (plist-get note :end))
                        (1+ (line-end-position))))))
              (overlay-put o 'audit-overlay t)
              (overlay-put o 'audit-item note)
              (overlay-put o 'priority 999999)
              (overlay-put o 'face
                           (cl-case (plist-get note :type)
                             (comment 'audit-comment-face)
                             (ok 'audit-ok-face)))
              (when (plist-get note :comment)
                (overlay-put o 'before-string
                             (propertize
                              (with-temp-buffer
                                (insert (plist-get note :comment))
                                (fill-paragraph)
                                (insert "\n")
                                (buffer-string))
                              'face
                              'audit-heading-face))))))
        (audit-cache)))

(defun audit-git-commit ()
  "Get the current commit."
  (let* ((cmd "git rev-parse HEAD")
         (branch (audit-git-shell-line cmd)))
    (if (not branch)
        (error "Failed: %s" cmd)
      (replace-regexp-in-string "\\`refs/heads/" "" branch))))

(defun audit-git-shell-line (cmd)
  "Run a command on the shell and return the first line."
  (car (split-string
        (shell-command-to-string cmd)
        "\n")))

(defun audit-export-markdown ()
  "Export to markdown."
  (interactive)
  (let ((root default-directory)
        (pkg (read-from-minibuffer "Package: "))
        (ver (read-from-minibuffer "Version: ")))
    (switch-to-buffer-other-window (get-buffer-create "*audit-report*"))
    (erase-buffer)
    (let* ((inhibit-read-only t)
           (items (cl-remove-if
                   (lambda (item)
                     (or (eq 'ok (plist-get item :type))
                         (string-match "^\\.\\." (file-relative-name (plist-get item :file) root))))
                   (audit-cache)))
           (files (audit-status-calculate-files root)))
      (erase-buffer)
      (insert "# Audit\n\n"
              "Package: " pkg "\n\n"
              "Version: " ver "\n\n"
              "SHA256: " (replace-regexp-in-string
                          "[ ]+-" ""
                          (shell-command-to-string "find . -type f | xargs sha256sum | sha256sum")) "\n"
              "Date-Completed: " (format-time-string "%Y-%m-%d") "\n\n"
              "Comments: " (number-to-string (length items)) "\n\n"
      )
      (insert (format "## Verify this audit

Verify the document:

    $ git clone https://github.com/chrisdone/audits
    $ cd audits
    $ keybase pgp verify -i haskell/%s/%s.md -d haskell/%s/%s.sig

Verify the SHA256 of the package contents:

    $ stack unpack %s-%s
    $ cd %s-%s
    $ find . -type f | xargs sha256sum | sha256sum
"
              pkg ver pkg ver pkg ver pkg ver))
      (insert "\n##Summary\n\n")
      (insert "## Files\n\n")
      (audit-status-list-files files)
      (insert "\n## Comments\n\n"))
    (let ((comments nil))
      (mapc (lambda (item)
              (let ((absolute-file (plist-get item :file)))
                (let ((relative-file (file-relative-name absolute-file root)))
                  (when (and (not (string-match "^\\.\\.[\\/]" relative-file))
                             (string-match audit-file-pattern relative-file))
                    (when absolute-file
                      (let* ((file (plist-get item :file))
                             (start (plist-get item :start))
                             (end (plist-get item :end))
                             (type (plist-get item :type))
                             (buffer (find-file-noselect file))
                             (line-start
                              (with-current-buffer buffer
                                (goto-char start)
                                (line-number-at-pos)))
                             (line-end
                              (with-current-buffer buffer
                                (goto-char end)
                                (line-number-at-pos)))
                             ;; (url
                             ;;  (with-current-buffer buffer
                             ;;    (set-mark start)
                             ;;    (goto-char end)
                             ;;    (github-urls-current-file-url)))
                             (sample
                              (with-current-buffer buffer
                                (buffer-substring start end)))
                             (comment (plist-get item :comment))
                             (relative-file (file-relative-name file root)))
                        (when (string-match audit-file-pattern relative-file)
                          (unless (eq type 'ok)
                            (setq comments t)
                            (insert (format "### L%d\n\n@%s:%d\n\n%s\n\n```haskell\n%s\n```\n"
                                            line-start
                                            relative-file
                                            line-start
                                            ;; url
                                            comment
                                            sample))))))))))
            (audit-cache))
      (unless comments
        (insert "No comments.")))))

(defun audit-sha256-files (files)
  "Get the SHA256 of all the files and then SHA256 that."
  (with-temp-buffer
    (cl-case (apply #'call-process
                    "sha256sum"
                    nil
                    (list (current-buffer) nil)
                    (mapcar (lambda (p) (plist-get p :relative-file)) files))
      (0 (goto-char (point-min))
         (shell-command-on-region
          (point-min)
          (point-max)
          "sha256sum"
          nil
          t)
         (replace-regexp-in-string "[ ]*-" "" (buffer-string)))
      (1 nil))))

(define-derived-mode audit-status-mode
  help-mode "Audit-Status"
  "Major mode for audit-status.
 \\{audit-status-mode-map}"
  (setq buffer-read-only t))

(define-key audit-status-mode-map (kbd "g") 'audit-status)
(define-key audit-status-mode-map (kbd "e") 'audit-status-edit)
(define-key audit-status-mode-map (kbd "k") 'audit-status-ok)

(define-derived-mode audit-list-mode
  help-mode "Audit-List"
  "Major mode for audit-list.
 \\{audit-list-mode-map}"
  (setq buffer-read-only t))

(define-key audit-list-mode-map (kbd "g") 'audit-list)
(define-key audit-list-mode-map (kbd "e") 'audit-list-edit)
(define-key audit-list-mode-map (kbd "k") 'audit-list-ok)

(defun audit-status-edit ()
  (interactive)
  (let ((item (get-text-property (point) 'audit-status-item)))
    (setq audit-cache
          (mapcar (lambda (this)
                    (when (equal this item)
                      (plist-put
                       this
                       :comment
                       (read-from-minibuffer
                        "Comment: "
                        (plist-get this :comment))))
                    this)
                  audit-cache))
    (audit-save)
    (audit-status)))

(defun audit-list-edit ()
  (interactive)
  (let ((item (get-text-property (point) 'audit-status-item)))
    (setq audit-cache
          (mapcar (lambda (this)
                    (when (equal this item)
                      (plist-put
                       this
                       :comment
                       (read-from-minibuffer
                        "Comment: "
                        (plist-get this :comment))))
                    this)
                  audit-cache))
    (audit-save)
    (audit-list)))

(defun audit-status-ok ()
  (interactive)
  (let ((item (get-text-property (point) 'audit-status-item)))
    (setq audit-cache
          (mapcar (lambda (this)
                    (when (equal this item)
                      (plist-put this :type 'ok)
                      (plist-put this :comment nil))
                    this)
                  audit-cache))
    (audit-save)
    (audit-status)))

(defun audit-list-ok ()
  (interactive)
  (let ((item (get-text-property (point) 'audit-status-item)))
    (setq audit-cache
          (mapcar (lambda (this)
                    (when (equal this item)
                      (plist-put this :type 'ok)
                      (plist-put this :comment nil))
                    this)
                  audit-cache))
    (audit-save)
    (audit-list)))

(defun audit-status ()
  "Display a status buffer of all non-OK review comments."
  (interactive)
  (let ((root default-directory))
    (unless (string= (buffer-name)
                     "*audit-status*")
      (switch-to-buffer-other-window (get-buffer-create "*audit-status*")))
    (setq default-directory root)
    (audit-status-mode)
    (let* ((inhibit-read-only t)
           (items (cl-remove-if
                   (lambda (item)
                     (or (eq 'ok (plist-get item :type))
                         (string-match "^\\.\\." (file-relative-name (plist-get item :file) root))))
                   (audit-cache)))
           (files (audit-status-calculate-files root)))
      (erase-buffer)
      (insert "Audit for directory: " root "\n"
              "Comments: "
              (number-to-string (length items))
              "\n"
              "Progress: "
              (if files
                  (format "%2.1f%%"
                          (min 100
                               (/ (cl-reduce '+
                                             (mapcar (lambda (x) (plist-get x :percent)) files)
                                             :initial-value 0.0)
                                  (length files))))
                "No files")
              "\n\n")
      (insert "Recent items:\n\n")
      (audit-status-list-items root items 3)
      (insert "Files:\n\n")
      (audit-status-list-files files)
      (goto-char (point-min)))
    (message "Audit refreshed.")))

(defun audit-list ()
  "Display all non-OK review comments."
  (interactive)
  (let ((root default-directory))
    (unless (string= (buffer-name)
                     "*audit-list*")
      (switch-to-buffer-other-window (get-buffer-create "*audit-list*")))
    (setq default-directory root)
    (audit-list-mode)
    (let* ((inhibit-read-only t)
           (items (cl-remove-if
                   (lambda (item)
                     (or (eq 'ok (plist-get item :type))
                         (string-match "^\\.\\." (file-relative-name (plist-get item :file) root))))
                   (audit-cache)))
           (files (audit-status-calculate-files root)))
      (let ((line (line-number-at-pos)))
        (erase-buffer)
        (insert "Audit for directory: " root "\n"
                "Comments: "
                (number-to-string (length items))
                "\n\n")
        (audit-status-list-items root items (length items))
        (goto-char (point-min))
        (forward-line (1- line))))
    (message "Audit refreshed.")))

(defun audit-status-list-files (files)
  "Insert the list of files."
  (mapc
   (lambda (stats)
     (insert (format "   %4.0f%% %4d / %4d "
                     (min 100 (plist-get stats :percent))
                     (plist-get stats :inspected-lines)
                     (plist-get stats :file-lines)))
     (let ((button (insert-button (plist-get stats :relative-file))))
       (button-put button 'path (plist-get stats :absolute-file))
       (button-put
        button 'action
        (lambda (button)
          (let ((file (button-get button 'path)))
            (find-file-other-window file)
            (audit-refresh)))))
     (insert "\n"))
   (sort files
         (lambda (x y)
           (let ((p1 (plist-get x :percent))
                 (p2 (plist-get y :percent)))
             (or (< p1 p2)
                 (when (= p1 p2)
                   (< (plist-get x :file-lines)
                      (plist-get y :file-lines)))))))))

(defun audit-status-list-items (root items count)
  "List N items."
  (mapc (lambda (item)
          (when (plist-get item :file)
            (let* ((file (plist-get item :file))
                   (start (plist-get item :start))
                   (end (plist-get item :end))
                   (type (plist-get item :type))
                   (buffer (find-file-noselect file))
                   (line-start
                    (with-current-buffer buffer
                      (goto-char start)
                      (line-number-at-pos)))
                   (line-end
                    (with-current-buffer buffer
                      (goto-char end)
                      (line-number-at-pos)))
                   (sample
                    (with-current-buffer buffer
                      (buffer-substring start end)))
                   (comment (plist-get item :comment)))
              (unless (eq type 'ok)
                (let ((button (insert-button (file-relative-name file root))))
                  (button-put button 'path (cons (file-relative-name file root) line-start))
                  (button-put button 'action
                              (lambda (button)
                                (let ((file-line (button-get button 'path)))
                                  (find-file-other-window (car file-line))
                                  (goto-char (point-min))
                                  (forward-line (1- (cdr file-line)))))))
                (insert (format ":%d" line-start)
                        "\n"
                        (with-temp-buffer
                          (insert (propertize
                                   (concat (or comment "No comment.") "\n")
                                   'audit-status-item item
                                   'face 'audit-heading-face))
                          (fill-paragraph)
                          (buffer-string))
                        (with-temp-buffer
                          (insert sample)
                          (delete-trailing-whitespace (point-min) (point-max))
                          (buffer-substring (point-min) (min (point-max) 128)))
                        "\n\n")))))
        (let ((list (cl-remove-if (lambda (item) (eq 'ok (plist-get item :type))) items)))
          (cl-subseq
           list
           0
           (min (length list) count)))))

(defun audit-status-calculate-files (root)
  (cl-remove-if-not
   #'identity
   (mapcar
    (lambda (absolute-file)
      (let ((relative-file (file-relative-name absolute-file root)))
        (when (string-match audit-file-pattern relative-file)
          (when absolute-file
            (with-current-buffer (find-file-noselect absolute-file)
              (save-excursion
                (let* ((file-lines
                        (progn
                          (goto-char (point-max))
                          (setq lines (line-number-at-pos))))
                       (inspected-lines
                        (cl-reduce
                         '+
                         (mapcar
                          (lambda (item)
                            (let* ((start (plist-get item :start))
                                   (end (plist-get item :end))
                                   (type (plist-get item :type))
                                   (line-start
                                    (progn
                                      (goto-char start)
                                      (line-number-at-pos)))
                                   (line-end
                                    (progn
                                      (goto-char end)
                                      (line-number-at-pos))))
                              (1+ (- line-end line-start))))
                          (cl-remove-if-not
                           (lambda (item)
                             (string= (plist-get item :file) (buffer-file-name)))
                           (audit-cache)))
                         :initial-value 0)))
                  (list
                   :percent (* 100.0 (/ (float inspected-lines) (float file-lines)))
                   :relative-file relative-file
                   :absolute-file absolute-file
                   :file-lines file-lines
                   :inspected-lines (min file-lines inspected-lines)))))))))
    (directory-files-recursively root ".*"))))

;;;;;;;;;;;;;;;;;;;;;
;; Introduced in 25.1

(when (not (fboundp 'directory-files-recursively))
  (defun directory-files-recursively (dir match &optional include-directories)
    "Return all files under DIR that have file names matching MATCH (a regexp).
This function works recursively.  Files are returned in \"depth first\"
and alphabetical order.
If INCLUDE-DIRECTORIES, also include directories that have matching names."
    (let ((result nil)
          (files nil)
          ;; When DIR is "/", remote file names like "/method:" could
          ;; also be offered.  We shall suppress them.
          (tramp-mode (and tramp-mode (file-remote-p dir))))
      (dolist (file (sort (file-name-all-completions "" dir)
                          'string<))
        (unless (member file '("./" "../"))
          (if (directory-name-p file)
              (let* ((leaf (substring file 0 (1- (length file))))
                     (full-file (expand-file-name leaf dir)))
                ;; Don't follow symlinks to other directories.
                (unless (file-symlink-p full-file)
                  (setq result
                        (nconc result (directory-files-recursively
                                       full-file match include-directories))))
                (when (and include-directories
                           (string-match match leaf))
                  (setq result (nconc result (list full-file)))))
            (when (string-match match file)
              (push (expand-file-name file dir) files)))))
      (nconc result (nreverse files)))))

(when (not (fboundp 'directory-name-p))
  (defsubst directory-name-p (name)
    "Return non-nil if NAME ends with a slash character."
    (and (> (length name) 0)
         (char-equal (aref name (1- (length name))) ?/))))

(provide 'audit)
