;;; quickjump.el ---

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

(defvar quickjump-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for using `intero-highlight-uses-mode'.")

(define-minor-mode quickjump-mode
  "Minor mode for quickly jumping to points in the buffer based on a simplistic prefix system."
  :lighter " ->"
  :keymap quickjump-mode-map
  (when (not quickjump-mode)
    (progn (setq buffer-read-only quickjump-buffer-access)
           (remove-overlays (point-min) (point-max) 'quickjump t))))

(defvar quickjump-origin nil)
(defvar quickjump-buffer-access nil)

(defun quickjump ()
  (interactive)
  (setq quickjump-buffer-access buffer-read-only)
  (setq buffer-read-only t)
  (setq quickjump-origin (point))
  (let ((start (window-start (get-buffer-window (current-buffer))))
        (end (window-end (get-buffer-window (current-buffer))))
        (hash (make-hash-table :test 'equal))
        (points nil)
        (doing t)
        (prefix "")
        (origin (point)))
    (remove-overlays start end 'quickjump t)
    (setq quickjump-count 0)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (save-excursion
          (let ((last nil))
            (while (and (< (point) end) (not (eq (point) last)))
              (setq last (point))
              (setq points (quickjump-point hash points t origin)))))
        (let ((last nil))
          (while (and (> (point) start) (not (eq (point) last)))
            (setq last (point))
            (setq points (quickjump-point hash points nil origin))))))
    (while doing
      (let ((key (key-description
                  (vector
                   (read-key
                    (concat (propertize "Prefix: " 'face 'minibuffer-prompt) prefix))))))
        (cond
         ((string= key "RET")
          (quickjump-mode -1)
          (setq doing nil))
         ((string= key "DEL")
          (setq prefix (quickjump-take (1- (length prefix)) prefix)))
         ((string= key "C-g")
          (goto-char origin)
          (quickjump-mode -1)
          (keyboard-quit))
         ((= 1 (length key))
          (setq prefix (concat prefix key))
          (quickjump-jump start end points prefix origin)))))))

(defun quickjump-jump (start end points prefix origin)
  (let ((point
         (let ((case-fold-search nil))
           (cdr
            (car
             (sort
              (cl-remove-if-not
               (lambda (key-point)
                 (string-match (concat "^" (regexp-quote prefix)) (car key-point)))
               points)
              (lambda (x y)
                (if (= (length (car x)) (length (car y)))
                    (or (string= (car x) prefix)
                        (< (abs (- (cdr x) (point))) (abs (- (cdr y) (point)))))
                  (string< (car x) (car y))))))))))
    (if point
        (progn (goto-char point)
               (mapc
                (lambda (o)
                  (when (overlay-get o 'quickjump)
                    (if (= point (overlay-start o))
                        (progn
                          (overlay-put o 'quickjump-current t)
                          (overlay-put o 'display (overlay-get o 'display-current)))
                      (when (overlay-get o 'quickjump-current)
                        (overlay-put o 'quickjump-current nil)
                        (overlay-put o 'display (overlay-get o 'display-lazy))))))
                (overlays-in start end)))
      (goto-char origin))))

(defun quickjump-point (hash points forward ignore-this-point)
  (let* ((origin (point))
         (start
          (if forward
              origin
            (progn (forward-same-syntax -1)
                   (when (= (point) origin)
                     (forward-word -1))
                   (point))))
         (end
          (if forward
              (progn (forward-same-syntax)
                     (when (= (point) origin)
                       (forward-word 1))
                     (point))
            origin)))
    (if (invisible-p (point))
        points
      (let ((o (make-overlay start end)))
        (overlay-put o 'priority 999)
        (overlay-put o 'quickjump t)
        (let* ((postfix (buffer-substring start end))
               (key (downcase (quickjump-take 2 postfix)))
               (prefix (quickjump-guid hash key nil)))
          (if (and (>= (length postfix) (- (length prefix) 1))
                   (not (string-match "[ \n]" postfix))
                   (not (= start ignore-this-point)))
              (let ((display-lazy
                     (concat (propertize prefix 'face 'lazy-highlight)
                             (propertize
                              (substring
                               postfix
                               (min (length postfix) (length prefix)))
                              'face 'quickjump-face-background))))
                (quickjump-guid hash key t)
                (overlay-put o 'display-lazy display-lazy)
                (overlay-put o 'display display-lazy)
                (overlay-put
                 o
                 'display-current
                 (concat (propertize prefix 'face 'isearch)
                         (propertize (substring postfix
                                                (min (length postfix) (length prefix)))
                                     'face 'quickjump-face-background)))
                (setq quickjump-count (+ 1 quickjump-count))
                (cons (cons prefix start) points))
            (progn (overlay-put o 'display (propertize postfix 'face 'quickjump-face-background))
                   points)))))))

(defun quickjump-guid (hash string write)
  (let ((count (gethash string hash 0)))
    (when write (puthash string (1+ count) hash))
    (concat string
            (if (= 0 count)
                ""
              (format "%x" count)))))

(defun quickjump-drop (n s)
  (substring s
             (min n (length s))))

(defun quickjump-take (n s)
  (if (>= (length s) n)
      (substring s 0 n)
    s))

(defface quickjump-face-background
  '((((background dark)) (:foreground "#888888"))
    (((background light)) (:foreground "#999999")))
  "Background face."
  :group 'quickjump)

(defun quickjump-mode-next ()
  (interactive)
  (message "No action yet: next"))

(defun quickjump-mode-prev ()
  (interactive)
  (message "No action yet: prev"))

(defun quickjump-mode-stop-here ()
  (interactive)
  (message "No action yet: stop here"))

(defun quickjump-mode-search ()
  (interactive)
  (message "No action yet: numbers"))

(defun quickjump-mode-delete ()
  (interactive)
  (message "No action yet: delete"))

(provide 'quickjump)
