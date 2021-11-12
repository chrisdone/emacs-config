;;; watchexec-ghci.el --- Watch dir, re-run GHCi commands

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

(defcustom watchexec-ghci-extensions
  "hs"
  "Comma-separated list of extensions to watch.")

(defvar-local watchexec-ghci-process nil
  "Process that you can stop later.")

(defvar-local watchexec-ghci-commands nil
  "Commands that we're running.")

(defvar-local watchexec-ghci-repl-buffer nil
  "The REPL's buffer.")

(defun watchexec-ghci-stop ()
  (interactive)
  (kill-process watchexec-ghci-process))

(defun watchexec-ghci (directory-to-watch ghci-commands-to-run)
  "Start a process watching DIRECTORY-TO-WATCH, when any file
changes, insert GHCI-COMMANDS-TO-RUN (as a string) into the
buffer followed by hitting the RET key."
  (interactive "sDirectory to watch:
sGHCi command(s) to run: ")
  (let ((repl-buffer (current-buffer))
        (buffer (get-buffer-create
                 (generate-new-buffer-name "watchexec-ghci"))))
    (with-current-buffer buffer
      (setq watchexec-ghci-repl-buffer repl-buffer))
    (setq watchexec-ghci-commands ghci-commands-to-run)
    (setq watchexec-ghci-process
          (start-process
           "watchexec-ghci"
           buffer
           "watchexec"
           "--no-shell"
           "--exts"
           watchexec-ghci-extensions
           "--watch"
           directory-to-watch
           "echo"))
    (set-process-buffer
     watchexec-ghci-process
     buffer)
    (set-process-query-on-exit-flag
     watchexec-ghci-process
     t)
    (set-process-filter
     watchexec-ghci-process
     'watchexec-filter)))

(defun watchexec-filter (process string)
  "Handle outputs from the process."
  (when (and (bufferp (process-buffer process))
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (with-current-buffer watchexec-ghci-repl-buffer
        (goto-char (point-max))
        (insert watchexec-ghci-commands)
        (call-interactively (key-binding (kbd "RET")))))))

(provide 'watchexec-ghci)
