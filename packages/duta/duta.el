;;; duta.el --- Duta mail server

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

(defvar duta-server-url nil)
(defvar duta-username nil)
(defvar duta-password-file nil)

;; TODO: Can't seem to get this request to send properly.
;;
;; (defun duta-curl (method path)
;;   "Make a GET request to PATH."
;;   (unless duta-server-url (error "Need duta-server-url"))
;;   (unless duta-username (error "Need duta-username"))
;;   (unless duta-password-file (error "Need duta-password-file"))
;;   (unless (or (string= method "GET") (string= method "POST"))
;;     (error "method is invalid %s" method))
;;   (let ((url-request-method method)
;;         (url-request-extra-header
;;          `(("Accept" . "application/json")
;;            ("user" . ,duta-username)
;;            ("pass" . ,(with-temp-buffer (insert-file-contents duta-password-file)
;;                              (replace-regexp-in-string "^pass: " "" (buffer-string)))))))
;;     (let ((buffer (url-retrieve-synchronously (concat duta-server-url path) t)))
;;       (with-current-buffer buffer
;;         (goto-char (point-min))
;;         (search-forward-regexp "\r?\n\r?\n" nil nil 1)
;;         (json-read-from-string
;;          (decode-coding-string
;;           (buffer-substring-no-properties (point) (point-max))
;;           'utf-8))))))

(defun duta-curl (method path)
  (unless duta-server-url (error "Need duta-server-url"))
  (unless duta-username (error "Need duta-username"))
  (unless duta-password-file (error "Need duta-password-file"))
  (unless (or (string= method "GET") (string= method "POST"))
    (error "method is invalid %s" method))
  (let ((user duta-username)
        (pass-file duta-password-file)
        (server-url duta-server-url))
    (with-temp-buffer
      (call-process
       "curl"
       nil
       t
       nil
       (concat server-url path)
       "-X" method
       "-H" "Accept: application/json"
       "-H" (concat "user: " user)
       "-H" (concat "@" pass-file)
       "--silent")
      (buffer-string))))

(defun duta-get (path)
  "Make a GET request to PATH."
  (json-read-from-string (decode-coding-string (duta-curl "GET" path) 'utf-8)))

(defun duta-get-async (path)
  "Make a GET request to PATH, async."
  (decode-coding-string (duta-curl "GET" path) 'utf-8))

(provide 'duta)
