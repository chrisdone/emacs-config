;;; vosk.el ---

;; Copyright (c) 2022 Chris Done. All rights reserved.

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

(defcustom vosk-partial-hook nil
  "Hook for an incoming partial.")

(defun vosk-buffer ()
  "The vosk buffer."
  (get-buffer-create "*vosk*"))

(defun vosk-command ()
  "Should output stream of {\"partial\": \"some text\"}, other
  payloads are ignored."
  (list "/home/chris/Work/alphacephei/vosk-server/vosk-command.sh"))

(defun vosk-read-partial ()
  "Read the partial text from the buffer."
  (condition-case nil
      (let ((json (json-read)))
        (delete-region (point-min) (point))
        (when (listp json)
          (let ((partial (assoc 'partial json)))
            (when (consp partial)
              (cdr partial)))))
    (error nil)))

(defun vosk-sentinel (&rest args)
  "Handle process events."
  (message "vosk-sentinel: %S" args))

(defun vosk-filter (process string)
  "Handle partial transcription of words."
  (let ((buffer (current-buffer)))
    (with-current-buffer (vosk-buffer)
      (save-excursion
        (goto-char (point-max))
        (insert (replace-regexp-in-string "\r\n" "" string))
        (goto-char (point-min))
        (let ((partial (vosk-read-partial)))
          (while partial
            (run-hook-with-args 'vosk-partial-hook buffer partial)
            (setq partial (vosk-read-partial))))))))

(defun vosk-start ()
  "Start vosk."
  (interactive)
  (vosk-stop)
  (with-current-buffer (vosk-buffer) (erase-buffer))
  (make-process
   :name "vosk-process"
   :buffer (vosk-buffer)
   :command (vosk-command)
   :noquery t
   :connection-type 'pty
   :filter 'vosk-filter
   :sentinel 'vosk-sentinel
   :stderr (get-buffer-create "*Messages*")))

(defun vosk-stop ()
  "Stop the process with SIGINT."
  (interactive)
  (condition-case nil
      (interrupt-process (get-buffer-process (vosk-buffer)))
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NATO
;;
;; (add-hook 'vosk-partial-hook 'vosk-nato-handler)

(defvar vosk-nato-prev-fragment ""
  "The previous value of the partial.")

(defvar vosk-nato-prev-word ""
  "Previous word obtained.")

(defvar vosk-nato-keys
  '(("alfa" . "a")
    ("bravo" . "b")
    ("charlie" . "c")
    ("delta" . "d")
    ("echo" . "e")
    ("foxtrot" . "f")
    ("golf" . "g")
    ("hotel" . "h")
    ("india" . "i")
    ("juliette" . "j")
    ("kilo" . "k")
    ("lima" . "l")
    ("mike" . "m")
    ("november" . "n")
    ("oscar" . "o")
    ("papa" . "p")
    ("quebec" . "q")
    ("romeo" . "r")
    ("sierra" . "s")
    ("tango" . "t")
    ("uniform" . "u")
    ("victor" . "v")
    ("whiskey" . "w")
    ("x" . "x")
    ("yankee" . "y")
    ("zulu" . "z")
    ("stop" . "C-g")
    ("space" . "<space>")
    ("escape" . "<escape>")
    ("slash" . "/")
    ("back" . "DEL")
    ("left" . "<left>")
    ("right" . "<right>")
    ("up" . "<up>")
    ("down" . "<down>")
    ("comma" . ",")))

(defun vosk-nato-handler (buffer partial)
  "Hook handler implementing NATO phonetic alphabet."
  (prog1
      (when (> (length partial) 0)
        (let* ((fragment
                (if (and (> (length partial)
                            (length vosk-nato-prev-fragment))
                         (string= (substring partial 0 (length vosk-nato-prev-fragment))
                                  vosk-nato-prev-fragment))
                    (let ((next (substring partial (length vosk-nato-prev-fragment))))
                      next)
                  partial))
               (word (car (last (split-string fragment)))))
          (prog1 (when (not (string= word vosk-nato-prev-word))
                   (let ((key (cdr (assoc word vosk-nato-keys 'string=))))
                     (when key
                       (message "key=%s" key))))
            (setq vosk-nato-prev-word word))))
    (setq vosk-nato-prev-fragment partial)))

(provide 'vosk)
