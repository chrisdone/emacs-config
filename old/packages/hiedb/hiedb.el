;;; hiedb.el ---

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

;; Example .dir-locals.el
;;
;; ((haskell-mode . ((hiedb-bin . "/Users/chris/.cabal/bin/hiedb")
;;                   (hiedb-file . "/Users/chris/Work/username/project/.hie-db")
;;                   (hiedb-root . "/Users/chris/Work/username/project/src"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config

(defgroup hiedb nil
  "Group for hiedb."
  :group 'haskell)

(defcustom hiedb-bin
  nil
  "The binary path."
  :type 'string
  :group 'hiedb)

(defcustom hiedb-file
  nil
  "Path to the database."
  :type 'string
  :group 'hiedb)

(defcustom hiedb-root
  nil
  "The root directory of the source code."
  :type 'string
  :group 'hiedb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization variables

(defun hiedb-bin ()
  "Generate the `hiedb-bin' variable."
  (or hiedb-bin
      (error "Please configure hiedb-bin in your .dir-locals.el. See hiedb.el for an example.")))

(defun hiedb-file ()
  "Generate the `hiedb-file' variable."
  (or hiedb-file
      (error "Please configure hiedb-file in your .dir-locals.el. See hiedb.el for an example.")))

(defun hiedb-root ()
  "Generate the `hiedb-root' variable."
  (or hiedb-root
      (error "Please configure hiedb-root in your .dir-locals.el. See hiedb.el for an example.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic RPC call infra

(defun hiedb-call (&rest args)
  "Call hiedb with the given args, returning a string of the output."
  (let ((bin (hiedb-bin))
        (file (hiedb-file))
        (in-file nil)
        (display nil))
    (with-temp-buffer
      (let ((out-buffer (current-buffer)))
        (cl-case (apply #'call-process
                        (append (list bin
                                      in-file
                                      out-buffer
                                      display)
                                (list "-D" file)
                                args))
          (0 (with-current-buffer out-buffer (buffer-string)))
          (t (error "hiedb error: %s" (with-current-buffer out-buffer (buffer-string)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RPC calls

(defun hiedb-point-defs (module line column)
  "Get definitions of the identifier at the given location."
  (let* ((output (hiedb-call "point-defs" module (number-to-string line) (number-to-string column)))
         (lines (split-string output "\n" t))
         (locations
          (remove-if-not #'identity
                         (mapcar (lambda (line)
                                   (when (string-match "^\\([A-Za-z_'0-9.]+\\):\\([0-9]+\\):\\([0-9]+\\)" line)
                                     (list :module (match-string 1 line)
                                           :line (string-to-number (match-string 2 line))
                                           :column (1- (string-to-number (match-string 3 line))))))
                                 lines))))
    locations))

(defun hiedb-point-refs (module line column)
  "Get references of the identifier at the given location."
  (let* ((output (hiedb-call "point-refs" module (number-to-string line) (number-to-string column)))
         (lines (split-string output "\n" t))
         (locations
          (remove-if-not #'identity
                         (mapcar (lambda (line)
                                   (when (string-match "^\\([A-Za-z_'0-9.]+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)" line)
                                     (list :module (match-string 1 line)
                                           :line (string-to-number (match-string 2 line))
                                           :column (1- (string-to-number (match-string 3 line)))
                                           :end-line (string-to-number (match-string 4 line))
                                           :end-column (1- (string-to-number (match-string 5 line))))))
                                 lines))))
    locations))

(defun hiedb-point-types (module line column)
  "Get types of the identifier at the given location."
  (mapconcat #'identity
             (split-string (hiedb-call "point-types" module (number-to-string line) (number-to-string column))
                           "\n"
                           t)
             "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defun hiedb-call-by-point (func)
  "Call (FUNC module line colunn) given the current point in the current buffer."
  (let ((line (line-number-at-pos))
        ;; Columns in hiedb are 1-indexed. Emacs are 0-indexed.
        (column (1+ (current-column))))
    (apply func
           (list (haskell-guess-module-name)
                 line
                 column))))

(defun hiedb-module-filepath (module)
  "Get the filepath of MODULE."
  (format "%s/%s.hs"
          (hiedb-root)
          (replace-regexp-in-string "\\." "/" module)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun hiedb-goto-def ()
  "Jump to the definition of thing at point."
  (interactive)
  (let* ((locations (hiedb-call-by-point 'hiedb-point-defs))
         (location (car locations)))
    (when location
      (when (fboundp 'xref-push-marker-stack) ;; Emacs 25
        (xref-push-marker-stack))
      (find-file (hiedb-module-filepath (plist-get location :module)))
      (goto-char (point-min))
      (forward-line (1- (plist-get location :line)))
      (goto-char (line-beginning-position))
      (forward-char (plist-get location :column)))))

(defun hiedb-show-type ()
  "Show type of thing at point."
  (interactive)
  (let ((types (hiedb-call-by-point 'hiedb-point-types)))
    (when types
      (message "%s" types))))

(provide 'hiedb)
