
;; Fundamental functions

(defun emacs-lisp-expand-clever (arg)
  "Cleverly expand symbols with normal dabbrev-expand, but also
  if the symbol is -foo, then expand to module-name-foo."
  (interactive "*P")
  (let ((sym (dabbrev--abbrev-at-point)))
    (cond
     ((string-prefix-p "-" sym)
      (let ((namespace (emacs-lisp-module-name)))
        (when namespace
          (save-excursion
            (backward-sexp 1)
            (insert namespace))))
      (dabbrev-expand arg))
     (t (dabbrev-expand arg)))))

(defun emacs-lisp-module-name ()
  "Search the buffer for `provide' declaration."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "^(provide '" nil t 1)
      (symbol-name (symbol-at-point)))))

(defun emacs-lisp-module-template ()
  (interactive)
  (when (and (buffer-file-name)
             (= (point-min)
                (point-max)))
    (let ((module-name
         (replace-regexp-in-string
          "\\.el$" ""
          (file-name-nondirectory (buffer-file-name)))))
    (insert (format ";;; %s.el --- $DESC$

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

%s

\(provide '%s)" module-name (if (string-match "^shm-" module-name)
                                "(require 'shm-core)\n\n" "") module-name))
    (search-backward "$DESC$")
    (delete-region (point)
                   (line-end-position)))))


;; Keybindings

(define-key emacs-lisp-mode-map (kbd "M-/") 'emacs-lisp-expand-clever)


;; Hooks

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-module-template)
