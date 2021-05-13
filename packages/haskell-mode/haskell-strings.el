;;; haskell-strings.el ---

;; Copyright (c) 2021 Chris Done. All rights reserved.

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

(defun haskell-paste-string ()
  "Paste a string into the buffer, replacing newlines and
speechmarks with escapes to ensure a clean paste."
  (interactive)
  (insert (replace-regexp-in-string
           "\n" "\\\\n"
           (replace-regexp-in-string
            "\"" "\\\\\""
            (with-temp-buffer (clipboard-yank)
                              (buffer-string))))))

(defun haskell-copy-string (beg end)
  "Copy a string from the buffer."
  (interactive "r")
  (let ((string (replace-regexp-in-string
                 "\\\\\n[ ]+\\\\" ""
                 (replace-regexp-in-string
                  "\\\\n" "\n"
                  (replace-regexp-in-string
                   "\\\\\"" "\""
                   (buffer-substring-no-properties beg end))))))
    (with-temp-buffer
      (insert string)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun shm/copy-string ()
  "Copy the string in the node."
  (interactive)
  (let ((string (shm-kill-node 'buffer-substring-no-properties nil nil t)))
    (with-temp-buffer
      (insert (replace-regexp-in-string
               "\\\\\n[ ]+\\\\" ""
               (replace-regexp-in-string
                "\\\\\"" "\""
                (replace-regexp-in-string
                 "\\\\n" "\n"
                 (replace-regexp-in-string
                  "\"$" ""
                  (replace-regexp-in-string
                   "^\"" ""
                   string))))))
      (clipboard-kill-ring-save (point-min) (point-max)))))

(provide 'haskell-strings)
