;;; projects-mode.el --- List status of all projects.

;; Copyright (c) 2014 Chris Done. All rights reserved.

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

(define-derived-mode projects-mode help-mode "Projects"
  "Major mode for viewing project status."
  (setq buffer-read-only t)
  (setq font-lock-defaults '(projects-mode-keywords))
  (projects-revert))

(define-key projects-mode-map (kbd "g") 'projects-revert)
(define-key projects-mode-map (kbd "n") 'projects-mode-next)
(define-key projects-mode-map (kbd "p") 'projects-mode-prev)
(define-key projects-mode-map (kbd "RET") 'projects-mode-go)

(defvar projects-mode-keywords
  '((".* has unpushed .*" . 'projects-mode-unpushed-face)
    (".* has untracked .*" . 'projects-mode-untracked-face)
    (".* has unstaged .*" . 'projects-mode-unstaged-face)
    (".* has uncommitted .*" . 'projects-mode-uncommitted-face))
  "Keywords for the different statuses.")

(defface projects-mode-unpushed-face
  '((((class color)) :foreground "#8191a2"))
  "Unpushed docs."
  :group nil)

(defface projects-mode-untracked-face
  '((((class color)) :foreground "#81a29b"))
  "Untracked docs."
  :group nil)

(defface projects-mode-unstaged-face
  '((((class color)) :foreground "#ede5bf"))
  "Unstaged docs."
  :group nil)

(defface projects-mode-uncommitted-face
  '((((class color)) :foreground "#d2cfc1"))
  "Unpushed docs."
  :group nil)

(defun projects ()
  (interactive)
  (switch-to-buffer "*projects*")
  (projects-mode))

(defun projects-revert ()
  "Revert list."
  (interactive)
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (goto-char (point-min))
    (replace-regexp " has .*$" " ... ")
    (goto-line line)
    (redisplay)
    (let ((str (shell-command-to-string "projects")))
      (erase-buffer)
      (loop for line in (remove-if (lambda (s) (string= "" s)) (split-string str "\n"))
            do (insert (propertize (replace-regexp-in-string "^[^ ]+ " "" line)
                                   'fp
                                   (replace-regexp-in-string "^\\([^ ]+\\) .*" "\\1" line))
                       "\n"))
      (sort-lines nil (point-min) (point-max)))
    (goto-line line)))

(defun projects-mode-go ()
  "Go to the repo at point."
  (interactive)
  (magit-status (get-text-property (point) 'fp)))

(defun projects-mode-next ()
  "Go to the repo at point."
  (interactive)
  (forward-line 1))

(defun projects-mode-prev ()
  "Go to the repo at point."
  (interactive)
  (forward-line -1))

(provide 'projects-mode)
