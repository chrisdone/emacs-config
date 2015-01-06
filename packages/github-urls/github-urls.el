;;; github-urls.el --- Get Github URLs

;; Copyright (c) 2014 Chris Done, Syohei YOSHIDA

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

(defvar github-urls-remote-regex
  "[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?\\'"
  "Regex used to match on remotes.")

(defun github-urls-current-file-url-copy ()
  "Copy the current file URL to the clipboard."
  (interactive)
  (github-urls-copy (github-urls-current-file-url)))

(defun github-urls-copy (string)
  "Copy some string to the clipboard."
  (with-temp-buffer
    (insert string)
    (clipboard-kill-region (point-min)
                           (point-max))))

(defun github-urls-current-file-url ()
  "Get the current file's Github URL."
  (let ((remote-url (github-urls-remote-url)))
    (github-urls-file-url (github-urls-user remote-url)
                          (github-urls-repo remote-url)
                          (github-urls-branch)
                          (github-urls-current-file-name)
                          (github-urls-current-marker))))

(defun github-urls-current-file-name ()
  "Get the current file name relative to the repo root."
  (file-relative-name (expand-file-name buffer-file-truename) ;; This works with symbolic links, too.
                      (github-urls-root)))

(defun github-urls-current-marker ()
  "Get a marker for the current buffer."
  (if (region-active-p)
      (format "#L%s..L%s"
              (save-excursion (goto-char (region-beginning))
                              (line-number-at-pos))
              (save-excursion (goto-char (region-end))
                              (line-number-at-pos)))
    (format "#L%s" (line-number-at-pos))))

(defun github-urls-remote-url ()
  "Get the current branch's remote URL."
  (github-urls-shell-line "git config --get remote.origin.url"))

(defun github-urls-shell-line (cmd)
  "Run a command on the shell and return the first line."
  (car (split-string
        (shell-command-to-string cmd)
        "\n")))

(defun github-urls-file-url (user repo branch file marker)
  "Make a URL for the file."
  (format "https://github.com/%s/%s/blob/%s/%s%s"
          user repo branch file marker))

(defun github-urls-issues-url (user repo)
  "Make a URL for the issue tracker."
  (format "https://github.com/%s/%s/issues"
          user repo))

(defun github-issues-open ()
  "Open the URL for the issue tracker."
  (interactive)
  (let ((remote-url (github-urls-remote-url)))
    (browse-url (github-urls-issues-url (github-urls-user remote-url)
                                        (github-urls-repo remote-url)))))

(defun github-urls-user (remote-url)
  "Return the user of the URL."
  (progn (string-match github-urls-remote-regex remote-url)
         (match-string 1 remote-url)))

(defun github-urls-repo (remote-url)
  "Return the repo of the URL."
  (progn (string-match github-urls-remote-regex remote-url)
         (match-string 2 remote-url)))

(defun github-urls-branch ()
  "Get the current branch."
  (let* ((cmd "git symbolic-ref HEAD")
         (branch (github-urls-shell-line cmd)))
    (if (not branch)
        (error "Failed: %s" cmd)
      (replace-regexp-in-string "\\`refs/heads/" "" branch))))

(defun github-urls-root ()
  "Get the root path of the repo."
  (let ((root (github-urls-shell-line "git rev-parse --show-toplevel")))
    (if (not root)
        (error "Error: not in a Git repository!")
      (file-name-as-directory root))))

(defun github-ticket-open (&optional ticket)
  "Open the ticket number at point."
  (interactive)
  (let ((number (or ticket
                    (github-get-ticket))))
    (unless (string= number "")
      (browse-url (concat github-ticket-prefix number)))))

(defun github-get-ticket ()
  "Get the ticket number at point."
  (save-excursion
    (when (looking-at "#")
      (forward-char))
    (search-backward-regexp "[^0-9]" (line-beginning-position) t 1)
    (forward-char)
    (let* ((start (point))
           (number (progn (search-forward-regexp "[0-9]+" (line-end-position) t)
                          (buffer-substring-no-properties start
                                                          (point)))))
      number)))

(provide 'github-urls)
