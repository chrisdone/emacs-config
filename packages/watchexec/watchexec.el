;;; watchexec.el --- Watch dir, run commands in a buffer (shell-mode/comint) when files change.
;;
;; Requires watchexec https://github.com/watchexec/watchexec

;; Copyright (c) 2021 Chris Done. All rights reserved.

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

(defcustom watchexec-extensions
  "hs,js,purs,txt"
  "Comma-separated list of extensions to watch.")

(defvar-local watchexec-process nil
  "Process that you can stop later.")

(defvar-local watchexec-commands nil
  "Commands that we're running.")

(defvar-local watchexec-repl-buffer nil
  "The REPL's buffer.")

(defcustom watchexec-bin-path
  "watchexec"
  "Binary path.")

(defun watchexec-stop ()
  (interactive)
  (kill-process watchexec-process))

(defun watchexec-set-command (-commands-to-run)
  (interactive "sCommand to run in buffer: ")
  (setq watchexec-commands -commands-to-run))

(defun watchexec (directory-to-watch -commands-to-run)
  "Start a process watching DIRECTORY-TO-WATCH, when any file
changes, insert -COMMANDS-TO-RUN (as a string) into the
buffer followed by hitting the RET key."
  (interactive (list (read-file-name "Directory to watch: ")
                     (read-from-minibuffer "Command to run: "
                                           (if (eq major-mode 'shell-mode)
                                               (ring-ref comint-input-ring 0)
                                             nil))))
  (let ((repl-buffer (current-buffer))
        (buffer (get-buffer-create
                 (generate-new-buffer-name "watchexec"))))
    (with-current-buffer buffer
      (setq watchexec-repl-buffer repl-buffer))
    (setq watchexec-commands -commands-to-run)
    (setq watchexec-process
          (start-process
           "watchexec"
           buffer
           watchexec-bin-path
           "--no-shell"
           "--exts"
           watchexec-extensions
           "--watch"
           directory-to-watch
           "echo"))
    (set-process-buffer
     watchexec-process
     buffer)
    (set-process-query-on-exit-flag
     watchexec-process
     t)
    (set-process-filter
     watchexec-process
     'watchexec-filter)))

(defun watchexec-filter (process string)
  "Handle outputs from the process."
  (when (and (bufferp (process-buffer process))
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (when (and (bufferp watchexec-repl-buffer)
                 (buffer-live-p watchexec-repl-buffer))
        (with-current-buffer watchexec-repl-buffer
          (goto-char (point-max))
          (insert watchexec-commands)
          (call-interactively (key-binding (kbd "RET"))))))))

(provide 'watchexec)
