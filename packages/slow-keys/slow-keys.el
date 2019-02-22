;;; slow-keys.el --- Slow keys mode to avoid RSI

;; Copyright (c) 2018 Chris Done. All rights reserved.

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

(define-minor-mode slow-keys-mode "Type slowly to avoid RSI."
  :lighter " Slow"
  :global t
  (if (bound-and-true-p slow-keys-mode)
      (add-hook 'post-command-hook 'slow-keys-do)
    (remove-hook 'post-command-hook 'slow-keys-do)))

(defvar slow-keys-repeat 0)
(defvar slow-keys-last-press 0)
(defvar slow-keys-sleep-for 0.3)
(defvar slow-keys-min-delay 0.3)

(defun slow-keys-do ()
  (unless (or executing-kbd-macro
              (slow-keys-ignore-cmd this-command))
    (setq slow-keys-repeat
          (if (eq last-command this-command)
              (1+ slow-keys-repeat)
            0))
    (when (and (> slow-keys-repeat 3)
               (not (slow-keys-typing-cmd this-command)))
      (slow-keys-slow-down
       (format "Use repetition numbers or more high-level commands: %S" this-command)))
    (let ((now (float-time)))
      (cond
       ((and (slow-keys-typing-cmd this-command)
             (slow-keys-typing-cmd last-command)
             (< (- now slow-keys-last-press) 0.1))
        (slow-keys-slow-down "Slow down typing!"))
       ((and (not (slow-keys-typing-cmd this-command))
             (not (slow-keys-typing-cmd last-command))
             (< (- now slow-keys-last-press) slow-keys-min-delay))
        (slow-keys-slow-down "Slow down command running!")))
      (setq slow-keys-last-press now))))

(defun slow-keys-slow-down (msg)
  (message "%s" (propertize msg 'face 'compilation-error))
  (redisplay)
  (sleep-for slow-keys-sleep-for))

(defun slow-keys-typing-cmd (cmd)
  (or (eq cmd 'self-insert-command)
      (eq cmd 'org-self-insert-command)
      (eq cmd 'isearch-printing-char)))

(defun slow-keys-ignore-cmd (cmd)
  (or (eq cmd 'mwheel-scroll)
      (eq cmd 'my-down-mouse)
      (eq cmd 'isearch-repeat-forward)))

(provide 'slow-keys)
