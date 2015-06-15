;;; track-mode.el --- Track yourself

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

(define-derived-mode track-mode fundamental-mode "Track"
  "Major mode to track your habits and activities.")

(define-key track-mode-map (kbd "RET") 'track-mode-new-entry)

(defun track-mode-new-entry (&optional entry)
  (interactive)
  (goto-char (point-max))
  (unless (string-match
           "^ *$"
           (buffer-substring-no-properties (line-beginning-position)
                                           (line-end-position))
           )
    (insert "\n"))
  (insert (car (split-string (shell-command-to-string "date +\"%Y-%m-%dT%H:%M:%SZ\"") "\n"))
          " "
          (or entry "")))

(provide 'track-mode)
