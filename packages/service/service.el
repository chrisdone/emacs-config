;;; service.el --- Start services in tabs

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

(defcustom service-list
  '()
  "A list of services."
  :group 'service
  :type '(repeat (list (string :tag "Name")
                       (string :tag "Directory")
                       (string :tag "Executable path"))))

(defun service/start (name)
  "Start a service."
  (interactive (list (ido-completing-read "Service: "
                                          (mapcar 'car service-list))))
  (let* ((entry (assoc name service-list))
         (default-directory (cadr entry))
         (buffer (get-buffer-create (format "*service:%s*" name))))
    (switch-to-buffer buffer)
    (shell buffer)
    (insert (caddr entry))
    (comint-send-input nil t)))

(defun service/restart (name)
  "Restart a service."
  (interactive (list (service-read-service)))
  (let* ((entry (assoc name service-list))
         (default-directory (cadr entry))
         (buffer (get-buffer (format "*service:%s*" name))))
    (if buffer
        (with-current-buffer buffer
          (comint-quit-subjob)
          (goto-char (point-max))
          (insert (caddr entry))
          (comint-send-input nil t))
      (service/start name))))

(defun service-read-service ()
  "Read from the services list."
  (ido-completing-read "Service: "
                       (mapcar 'car service-list)))

(provide 'service)
