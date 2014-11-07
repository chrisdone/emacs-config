;;; resmacro.el --- Resumable keyboard macro recording

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

(require 'cl)
(require 'edmacro)

(defvar resmacro-start-keys (vector)
  "The keys used to start macro recording.")

(defun resmacro-start-macro (arg)
  "A version of `kmacro-start-macro' which when ARG is passed
will resume recording with the keys pressed since it was last
invoked, prompting to remove any erroneous keys."
  (interactive "P")
  (if (and (not (or defining-kbd-macro executing-kbd-macro))
           arg)
      (let* ((keys (recent-keys))
             ;; Let's strip off the keys used to run this command.
             (unmuddied-keys
              (apply #'vector
                     (cl-loop for i from 0 to (1- (- (length keys)
                                                     (length (this-command-keys))))
                              collect (elt keys i)))))
        (let ((start-keys resmacro-start-keys))
          (setq resmacro-start-keys (this-command-keys))
          (resmacro-user-prompt-resume
           arg
           (resmacro-recent-keys start-keys
                                 unmuddied-keys))))
    (progn (setq resmacro-start-keys (this-command-keys))
           (kmacro-start-macro nil))))

(defun resmacro-show-key (ev)
  "Show a key without throwing an error for mouse events."
  (cond
   ((atom ev)
    (edmacro-format-keys (vector ev)))
   ((eq (car ev) 'help-echo)
    (edmacro-format-keys (vector ev)))
   ((eq (car ev) 'switch-frame)
    (edmacro-format-keys (vector ev)))
   ((equal ev '(menu-bar))
    (edmacro-format-keys (vector ev)))
   ((equal (cl-cadadr ev) '(menu-bar))
    (edmacro-format-keys (vector ev)))
   (t "<mouse-event>")))

(defun resmacro-user-prompt-resume (arg keys)
  "Prompt for the user to resume with the given incomplete
keyboard macro."
  (let ((done nil))
    (while (not done)
      (let ((key (read-event
                  (concat (propertize
                           "Resume macro with (RET: continue, DEL: remove last): "
                           'face 'minibuffer-prompt)
                          (mapconcat #'resmacro-show-key
                                     (cl-loop for i from 0 to (max 0 (- (length keys) 2))
                                              collect (elt keys i))
                                     " ")
                          " "
                          (propertize (resmacro-show-key
                                       (elt keys (1- (length keys))))
                                      'face 'bold)))))
        (case key
          (7
           (keyboard-quit))
          (backspace
           (setq keys
                 (apply #'vector
                        (cl-loop for i from 0 to (max 0 (- (length keys) 2))
                                 collect (elt keys i))))
           (when (= 0 (length keys))
             (setq done t)))
          (return
           (setq done t))))))
  (unless (= (length keys) 0)
    (setq last-kbd-macro keys)
    (kmacro-start-macro arg)))

(defun resmacro-recent-keys (prefix keys)
  "Get the keys of the current macro, by searching for the start
specified by PREFIX."
  (let ((j 0))
    (remove-if
     (lambda (k) (equal k 7)) ;; Strip out `C-g'.
     (if (= (length prefix) 0)
         keys
       (apply #'vector
              (let ((result (list)))
                (cl-loop for i from 0 to (1- (length keys))
                         ;; Constantly append to the buffer.
                         do (add-to-list 'result (elt keys i) t (lambda (_x _y) nil))
                         ;; Reset whenever the `prefix' is invalidated.
                         when (and (< j (length prefix))
                                   (not (equal (elt keys i) (elt prefix j))))
                         do (setq j 0)
                         ;; Reset whenever we encounter the start of the
                         ;; `prefix' (this assumes no repeated start keys).
                         when (equal (elt keys i) (elt prefix 0))
                         do (setq j 0)
                         ;; While prefix is being satisfied, increment `j'.
                         ;; Resets the `result' buffer when we've finished
                         ;; identifying the `prefix'.
                         when (and (< j (length prefix))
                                   (equal (elt keys i) (elt prefix j)))
                         do (progn (setq j (1+ j))
                                   (when (= j (length prefix))
                                     ;; If we're at the end of the vector, that means the last
                                     ;; key sequence pressed was our prefix. So let's strip that
                                     ;; off.
                                     (if (= i (1- (length keys)))
                                         (setq result (nbutlast result
                                                                (length prefix)))
                                       (setq result (list))))))
                result))))))

(defun resmacro-test-recent-keys ()
  "Run tests for `resmacro-recent-keys'."
  (interactive)
  (cl-loop for test in '((([] []) . [])
                         (([] [1]) . [1])
                         (([] [1 2]) . [1 2])
                         (([1] [1 2]) . [2])
                         (([1] [1]) . [])
                         (([1 2] [1 2]) . [])
                         (([1] [1 2 3 4 5]) . [2 3 4 5])
                         (([1 2] [1 2 3 4 5]) . [3 4 5])
                         (([1 2] [1 2 3 1 2 4 5]) . [4 5])
                         (([1 2] [1 2 3 1 2]) . [3])
                         (([1 2 3] [1 2]) . [1 2])
                         (([1 2 3] []) . [])
                         (([0 1] [right up 1 11 f3 102 111 111 7 21 0 1]) .
                          [right up 1 11 f3 102 111 111 21]))
           when (not (equalp (apply #'resmacro-recent-keys (car test))
                             (cdr test)))
           do (error "Test failed for %S:\n\nexpected: %S\nactual: %S"
                     `(resmacro-recent-keys ,(caar test) ,(cadar test))
                     (cdr test)
                     (apply #'resmacro-recent-keys (car test))))
  (message "OK."))

(provide 'resmacro)
