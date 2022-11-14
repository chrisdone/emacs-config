;;; org-multiclock.el --- Clock into multiple things at once

;; Copyright (c) 2017 Chris Done. All rights reserved.

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

(defconst org-multiclock-clock-current-regex
  "^[ ]+CLOCK: \\[\\([0-9]+-[0-9]+-[0-9]+ [A-Za-z]+ [0-9]+:[0-9]+\\)\\]$"
  "Regex matching a current CLOCK in.")

(defun org-multiclock-in ()
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (search-backward-regexp org-todo-line-regexp nil t 1)
    (goto-char (line-end-position))
    (let ((boundary (save-excursion
                      (if (search-forward-regexp org-todo-line-regexp nil t 1)
                          (max 0 (1- (line-beginning-position)))
                        (point-max)))))
      (if (search-forward-regexp org-multiclock-clock-current-regex boundary t 1)
          (message "Already clocked into this task.")
        (insert
         (format
          "\n  CLOCK: [%s]"
          (format-time-string "%Y-%m-%d %a %H:%M")))))))

(defun org-multiclock-out ()
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (search-backward-regexp org-todo-line-regexp nil t 1)
    (goto-char (line-end-position))
    (let ((boundary (save-excursion
                      (if (search-forward-regexp org-todo-line-regexp nil t 1)
                          (max 0 (1- (line-beginning-position)))
                        (point-max)))))
      (if (search-forward-regexp org-multiclock-clock-current-regex boundary t 1)
          (let ((start-time (org-multiclock-parse-time (match-string 1))))
            (insert
             (format
              "--[%s] => %s"
              (format-time-string "%Y-%m-%d %a %H:%M")
              (org-multiclock-format-hours
               (/ (- (float-time (current-time)) (float-time start-time))
                  3600)))))
        (message "Not clocked into this task.")))))

(defun org-multiclock-format-hours (hours)
  "Print a decimal number of hours into base 60 e.g. 2.5 hours is 2:30."
  (format "%2d:%02d" (floor hours) (* 60 (- hours (floor hours)))))

(defun org-multiclock-parse-time (string)
  "Parse the time STRING."
  (save-excursion
    (apply 'encode-time (org-parse-time-string string))))

(defun org-multiclock-parse-hours (string)
  "Parse STRING hh:mm into a number of hours."
  (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" string)
    (let ((hours (match-string 1 string))
          (minutes (match-string 2 string)))
      (string-to-number
       (format "%0.2f"
               (+ (string-to-number hours)
                  (/ (+ 0.0 (string-to-number minutes)) 60)))))))

(provide 'org-multiclock)
