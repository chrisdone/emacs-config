;;; tail-on-change.el --- Jump to tail on change

;; Copyright (c) 2016 Chris Done. All rights reserved.

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

(define-minor-mode tail-on-change-mode
  "Jump to the tail of the buffer when the contents of the buffer changes."
  nil " TailChanges" nil
  (if tail-on-change-mode
      (tail-on-change-add-hooks)
    (tail-on-change-remove-hooks)))

(defun tail-on-change-add-hooks ()
  (add-hook 'after-change-functions 'tail-on-change nil t))

(defun tail-on-change-remove-hooks ()
  (remove-hook 'after-change-functions 'tail-on-change t))

(defun tail-on-change (&rest _)
  (let ((window (get-buffer-window (current-buffer))))
    (when window
      (with-selected-window window
        (set-window-point window (point-max))
        (recenter -1)))))

(provide 'tail-on-change)
