;;; duta-thread-mode.el --- Thread viewer

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

(defvar-local duta-thread-mode-id nil)

(define-derived-mode duta-thread-mode
  fundamental-mode "Duta-Thread"
  "Major mode for showing a thread.
 \\{duta-thread-mode-map}"
  (setq buffer-read-only t)
  (hl-line-mode 1))

(define-key duta-thread-mode-map (kbd "g") 'duta-thread-refresh)
(define-key duta-thread-mode-map (kbd "d") 'duta-thread-delete)
(define-key duta-thread-mode-map (kbd "s") 'duta-thread-spam)
(define-key duta-thread-mode-map (kbd "q") 'duta-thread-quit)

(defface duta-thread-mode-subject-face
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark))
     :foreground "#8cd0d3")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defface duta-thread-mode-timestamp-face
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark))
     :foreground "#88b090")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defun duta-thread-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (insert "Refreshing ...\n\n")
    (redisplay)
    (let ((thread (duta-get (format "thread/%d" duta-thread-mode-id))))
      (save-excursion
        (erase-buffer)
        (insert (duta-thread-render-thread thread))))))

(defun duta-thread-render-thread (thread)
  (concat
   (duta-thread-render-header thread)
   "\n\n"
   (duta-thread-render-forest
    (cdr (assoc 'forest thread))
    (cdr (assoc 'plain_parts thread)))))

(defun duta-thread-render-header (thread)
  (concat
   (propertize (cdr (assoc 'subject thread)) 'face 'duta-thread-mode-subject-face)
   " "
   (format "(%d)" (cdr (assoc 'messages thread)))
   "\n"
   "Created: " (propertize (cdr (assoc 'created thread)) 'face 'duta-thread-mode-timestamp-face)
   "\n"
   "Updated: " (propertize (cdr (assoc 'updated thread)) 'face 'duta-thread-mode-timestamp-face)))

(defun duta-thread-render-forest (forest parts)
  (mapconcat
   (lambda (tree)
     (let* ((message (cdr (assoc 'message tree)))
            (id (cdr (assoc 'id tree)))
            (received (cdr (assoc 'received message)))
            (from_header (or (cdr (assoc 'from_header message))
                             (cdr (assoc 'mail_from_header message))))
            (to_header (or (cdr (assoc 'to_header message))
                           (cdr (assoc 'rcpt_to message)))))
       (concat
        from_header "\n"
        "Received: " (propertize received 'face 'duta-thread-mode-timestamp-face)
        "\n\n"
        (duta-thread-render-message-parts id parts)
        )))
   forest
   "\n\n"))

(defun duta-thread-render-message-parts (id parts)
  (mapconcat
   (lambda (part)
     (replace-regexp-in-string "" "" (cdr (assoc 'content part))))
   (remove-if-not
    (lambda (part)
      (= (cdr (assoc 'message part))
         id))
    parts)
   "\n"))

(defun duta-thread-buffer-name (thread-id)
  (format "*duta:thread-%d*" thread-id))

(defun duta-thread-delete ()
  (interactive)
  (duta-get-async (format "apply-label/%d/deleted" duta-thread-mode-id))
  (kill-buffer)
  (when (string= (buffer-name (current-buffer))
                 (buffer-name (get-buffer-create "*duta*")))
    (duta-threads-refresh)))

(defun duta-thread-spam ()
  (interactive)
  (duta-get-async (format "apply-label/%d/spam" duta-thread-mode-id))
  (kill-buffer)
  (when (string= (buffer-name (current-buffer))
                 (buffer-name (get-buffer-create "*duta*")))
    (duta-threads-refresh)))

(defun duta-thread-quit ()
  (interactive)
  (kill-buffer))

(provide 'duta-thread-mode)
