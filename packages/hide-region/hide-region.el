;;; hide-region.el --- Simply hide the region at point.

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

(defun hide-region (beg end)
  "Hide region."
  (interactive "r")
  (let ((o (make-overlay beg end)))
    (overlay-put o 'invisible t)
    (overlay-put o 'intangible t)
    (overlay-put o 'evaporate t)
    (overlay-put o 'after-string "...")
    (overlay-put o 'hide-region t)))

(defun hide-region-undo ()
  "Undo the hidden region at point."
  (interactive)
  (mapcar (lambda (o)
            (when (overlay-get o 'hide-region)
              (delete-overlay o)))
          (overlays-in (- (point) 1)
                       (+ (point) 1))))

(provide 'hide-region)
