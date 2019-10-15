;;; duta-threads-mode.el --- Threads listing mode

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

(require 'hl-line)

(defvar-local duta-threads-mode-path ""
  "Path to pull threads from.")

(defun duta ()
  "Start duta in buffer *duta*, listing threads in the inbox."
  (interactive)
  (with-current-buffer (duta-threads-buffer)
    (duta-threads-mode)
    (switch-to-buffer (current-buffer))))

(defun duta-threads-buffer ()
  (get-buffer-create "*duta*"))

(define-derived-mode duta-threads-mode
  fundamental-mode "Duta"
  "Major mode for listing threads.
 \\{duta-threads-mode-map}"
  (setq buffer-read-only t)
  (duta-threads-refresh)
  (hl-line-mode 1))

(define-key duta-threads-mode-map (kbd "g") 'duta-threads-refresh)
(define-key duta-threads-mode-map (kbd "RET") 'duta-threads-open)
(define-key duta-threads-mode-map (kbd "d") 'duta-threads-delete)
(define-key duta-threads-mode-map (kbd "s") 'duta-threads-spam)
(define-key duta-threads-mode-map (kbd "n") 'duta-threads-next)
(define-key duta-threads-mode-map (kbd "p") 'duta-threads-previous)
(define-key duta-threads-mode-map (kbd "h") 'duta-threads-previous)

(defface duta-threads-mode-unread-face
  '((((class color) (min-colors 88) (background light))
     :weight bold)
    (((class color) (min-colors 88) (background dark))
     :weight bold
     :foreground "#8cd0d3")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defface duta-threads-mode-deleted-face
  '((((class color) (min-colors 88) (background light))
     :weight normal
     :foreground "#cccccc")
    (((class color) (min-colors 88) (background dark))
     :weight normal
     :foreground "#999999")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defface duta-threads-mode-read-face
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark))
     :foreground "#8cd0d3")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defface duta-threads-mode-timestamp-face
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark))
     :foreground "#88b090")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defun duta-threads-refresh ()
  (interactive)
  (let ((inhibit-read-only t)
        (original-line (line-number-at-pos)))
    (insert " [Refreshing] ")
    (redisplay)
    (let ((threads (duta-get duta-threads-mode-path)))
      (erase-buffer)
      (when (= 0 (length threads))
        (insert "No threads to display!"))
      (mapc (lambda (thread)
              (let ((start (point)))
                (insert (duta-threads-render-thread thread))
                (add-text-properties start
                                     (point)
                                     (list 'duta-thread-begin start
                                           'duta-thread-end (point)
                                           'duta-thread-id (cdr (assoc 'id thread))))))
            threads)
      (goto-char (point-min))
      (forward-line (1- original-line)))))

(defun duta-threads-render-thread (thread)
  (let* ((tags (mapcar #'identity (cdr (assoc 'tags thread))))
         (unread (cl-remove-if-not
                  (lambda (tag)
                    (let ((label (cdr (assoc 'label tag))))
                      (string= label "unread")))
                  tags)))
    (format "%s (%d)\n%s\n\n"
            (propertize (cdr (assoc 'subject thread))
                        'face
                        (if unread
                            'duta-threads-mode-unread-face
                          'duta-threads-mode-read-face))
            (cdr (assoc 'messages thread))
            (propertize (cdr (assoc 'updated thread))
                        'face 'duta-threads-mode-timestamp-face))))

(defun duta-threads-thread-id ()
  (get-text-property (point) 'duta-thread-id))

(defun duta-threads-begin ()
  (get-text-property (point) 'duta-thread-begin))

(defun duta-threads-end ()
  (get-text-property (point) 'duta-thread-end))

(defun duta-threads-open ()
  (interactive)
  (switch-to-buffer
   (let ((thread-id (duta-threads-thread-id)))
     (with-current-buffer (get-buffer-create (duta-thread-buffer-name thread-id))
       (duta-thread-mode)
       (setq duta-thread-mode-id thread-id)
       (duta-thread-refresh)
       (current-buffer)))))

(defun duta-threads-delete ()
  (interactive)
  (duta-get-async (format "apply-label/%d/deleted" (duta-threads-thread-id)))
  (let ((inhibit-read-only t))
    (add-text-properties
     (duta-threads-begin)
     (duta-threads-end)
     (list 'face
           'duta-threads-mode-deleted-face))
    (duta-threads-next)))

(defun duta-threads-spam ()
  (interactive)
  (duta-get-async (format "apply-label/%d/spam" (duta-threads-thread-id)))
  (let ((inhibit-read-only t))
    (add-text-properties
     (duta-threads-begin)
     (duta-threads-end)
     (list 'face
           'duta-threads-mode-deleted-face))
    (duta-threads-next)))

(defun duta-threads-next ()
  (interactive)
  (when (duta-threads-end)
    (goto-char (duta-threads-end))))

(defun duta-threads-previous ()
  (interactive)
  (when (duta-threads-begin)
    (goto-char (1- (duta-threads-begin)))
    (when (duta-threads-begin)
      (goto-char (duta-threads-begin)))))

(provide 'duta-threads-mode)
