;;; decorum.el ---

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

(defun decorum-buffer ()
  (interactive)
  (decorum-region (point-min) (point-max)))

(defun decorum-region (beg end)
  (interactive "r")
  (save-excursion
    (let ((continue t))
      (goto-char beg)
      (while continue
        (setq continue nil)
        (when (search-forward-regexp (regexp-opt (list "{" "(" "[")) end t 1)
          (let ((start (point)))
            (forward-char -1)
            (forward-sexp 1)
            (forward-char -1)
            (let ((end (point)))
              (when (> (- end start) 1)
                (put-text-property start end 'invisible t)
                (put-text-property start end 'decorum t)
                (put-text-property (- start 1) (+ end 1) 'mouse-face 'button)
                (put-text-property (- start 1) (+ end 1) 'help-echo "Click to toggle region")
                (put-text-property (- start 1) (+ end 1) 'decorum-region (cons start end))
                (put-text-property (- start 1)
                                   (+ end 1)
                                   'keymap
                                   (let ((map (make-keymap)))
                                     (define-key map [mouse-1] 'decorum-click)
                                     (define-key map [mouse-2] 'decorum-click)
                                     map)))
              (setq continue t))))))))

(defun decorum-click ()
  (interactive)
  (let ((reg (get-text-property (point) 'decorum-region)))
    (if (get-text-property (car reg) 'invisible)
        (progn (message "Expanding region...")
               (put-text-property (car reg) (cdr reg) 'invisible nil)
               (put-text-property (car reg) (cdr reg) 'mouse-face nil)
               (put-text-property (car reg) (cdr reg) 'help-echo nil)
               (decorum-region (car reg) (cdr reg)))
      (progn
        (message "Hiding region...")
        (decorum-region (- (car reg) 1) (+ 1 (cdr reg)))))))

(provide 'decorum)
