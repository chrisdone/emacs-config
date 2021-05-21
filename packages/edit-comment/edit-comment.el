;;; edit-comment.el ---

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

(defun edit-comment (beg end)
  (interactive "r")
  (let* ((point (point))
         (start
          (or beg (let ((pos (line-beginning-position)))
             (save-excursion
               (while (string-match "comment"
                                    (symbol-name (get-text-property (line-beginning-position) 'face)))
                 (setq pos (line-beginning-position))
                 (forward-line -1)))
             pos)))
         (end
          (or end (let ((pos (line-beginning-position)))
             (save-excursion
               (while (string-match "comment"
                                    (symbol-name (get-text-property (line-end-position) 'face)))
                 (setq pos (line-end-position))
                 (forward-line 1)))
             pos)))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end)))
    (uncomment-region start end)
    (let* ((new-start (progn (goto-char (point-min))
                             (forward-line (1- start-line))
                             (line-beginning-position)))
           (new-end (progn (goto-char (point-min))
                           (forward-line (1- end-line))
                           (line-end-position)))
           (string (let ((s (buffer-substring new-start new-end)))
                     (delete-region new-start new-end)
                     s))
           (new-string
            (save-window-excursion
              (with-temp-buffer
                (rename-buffer (generate-new-buffer-name "edit-comment"))
                (use-local-map
                 (let ((map (copy-keymap widget-keymap)))
                   (define-key map (kbd "C-c C-c") 'exit-recursive-edit)
                   (define-key map (kbd "C-c C-k") 'abort-recursive-edit)
                   (define-key map (kbd "C-g")     'abort-recursive-edit)
                   map))
                (switch-to-buffer-other-window (current-buffer))
                (insert string)
                (goto-char (point-min))
                (recursive-edit)
                (buffer-substring-no-properties (point-min) (point-max))))))
      (when new-string
        (goto-char start)
        (insert new-string)
        (let ((comment-empty-lines t))
          (comment-region start (point)))
        (goto-char point)))))

(provide 'edit-comment)
