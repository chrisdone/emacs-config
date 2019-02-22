;;; slides.el --- Slides

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

(define-derived-mode slides-mode
  fundamental-mode "Slides"
  "Major mode for slides.
 \\{slides-mode-map}"
  (setq-local cursor-type nil)
  (add-hook 'focus-in-hook 'slides-focus-hook t)
  (add-hook 'buffer-list-update-hook 'slides-focus-hook t)
  (setq buffer-read-only t))
(defun slides-focus-hook ()
  (when (eq major-mode 'slides-mode)
    (setq-local cursor-type nil)))
(define-key slides-mode-map (kbd "SPC") 'slides-forward)
(define-key slides-mode-map (kbd "f") 'slides-forward)
(define-key slides-mode-map (kbd "b") 'slides-back)
(define-key slides-mode-map (kbd "C-f") 'slides-forward)
(define-key slides-mode-map (kbd "C-b") 'slides-back)
(define-key slides-mode-map (kbd "g") 'slides-refresh)
(define-key slides-mode-map (kbd "<backspace>") 'slides-back)
(defvar-local slides-current 0)
(defun slides-back ()
  (interactive)
  (setq slides-current (max 0 (- slides-current 1)))
  (slides-refresh)
  (slides-focus-hook))
(defun slides-forward ()
  (interactive)
  (setq slides-current (min (- (length slides) 1) (+ slides-current 1)))
  (slides-refresh)
  (slides-focus-hook))
(defun slides-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (progn (erase-buffer) (funcall (nth slides-current slides))))
  (slides-focus-hook))

(provide 'slides)

