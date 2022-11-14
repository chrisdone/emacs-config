;;; markup-mode.el --- For markup.

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

(define-derived-mode markup-mode
  fundamental-mode "Markup"
  "Major mode for markup.
 \\{markup-mode-map}")

(define-key markup-mode-map [remap self-insert-command] 'markup-mode-self-insert)

(defun markup-mode-self-insert ()
  "Self-insertion command."
  (interactive)
  (let ((key (aref (this-command-keys-vector)
                   (- (length (this-command-keys-vector)) 1))))
    (case key
      (?< (markup-mode-open-tag))
      (t (if (save-excursion (backward-char 1)
                             (and (looking-at "<>")
                                  (markup-mode-tag-name-char-p key)))
             (progn (call-interactively 'self-insert-command)
                    (save-excursion
                      (search-forward-regexp "</>")
                      (forward-char -1)
                      (call-interactively 'self-insert-command)))
           (if (and (markup-mode-in-tag-p)
                    (markup-mode-in-tag-name-p))
               (if (markup-mode-tag-name-char-p key)
                   (markup-mode-insert-tag-name-char)
                 (case key
                   (?# (markup-mode-do-tag-id))
                   (?. (markup-mode-do-tag-class))
                   (t (message "Ignoring key press at invalid position."))))
             (if (markup-mode-in-text-p)
                 (call-interactively 'self-insert-command)
               (message "Ignoring key press at invalid position."))))))))

(defun markup-mode-insert-tag-name-char ()
  (save-excursion
    (sgml-skip-tag-forward 1)
    (forward-char -1)
    (call-interactively 'self-insert-command))
  (call-interactively 'self-insert-command))

(defun markup-mode-do-tag-id ()
  "Jump to or insert a tag id."
  (unless (looking-back " ")
    (insert " "))
  (insert "id=\"\"")
  (unless (looking-at "[ >]")
    (insert " "))
  (forward-char -1))

(defun markup-mode-do-tag-class ()
  "Jump to or insert a tag class."
  (unless (looking-back " ")
    (insert " "))
  (insert "class=\"\"")
  (unless (looking-at "[ >]")
    (insert " "))
  (forward-char -1))

(defun markup-mode-in-text-p ()
  "Are we in text?"
  (let ((ctx (sgml-lexical-context)))
    (eq (car ctx) 'text)))

(defun markup-mode-tag-name-char-p (key)
  "Is the character a valid tag name character?"
  (string-match sgml-name-re (char-to-string key)))

(defun markup-mode-in-tag-p ()
  "Are we in a <tag>?"
  (let ((ctx (sgml-lexical-context)))
    (eq (car ctx) 'tag)))

(defun markup-mode-in-tag-name-p ()
  "Are we at the tag name?"
  (looking-back sgml-tag-name-re))

(defun markup-mode-open-tag ()
  "Insert an open tag."
  (save-excursion (insert "<></>"))
  (forward-char 1))

(provide 'markup-mode)
