;;; align-by-current-symbol.el
;;; Align lines containing a symbol according to that symbol.

;; Copyright (c) 2010 Chris Done. All rights reserved.

;; Author:   Chris Done <chrisdone@gmail.com>
;; Created:  14-May-2010
;; Version:  0.1
;; Keywords: convenience
;; X-URL:    http://emacswiki.org/align-by-current-symbol.el

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;; Chris Done BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Example usage:
;;
;; (load "align-by-current-symbol.el")
;; (global-set-key (kbd "C-c C-.") 'align-by-current-symbol)
;;
;; By default it requires spaces to be around the symbol.
;;
;; Use the following to turn this off:
;;
;; (global-set-key (kbd "C-c C-.")
;;    (lambda ()
;;       (interactive) (align-by-current-symbol t)))
;;
;; Example demonstration:
;;
;; mumumu = zotzot
;; chi = far
;; popo = k
;; zarlo => mu
;;
;; place point at `=' after `chi', hit C-c C-.:
;;
;; mumumu = zotzot
;; chi    = far
;; popo   = k
;; zarlo => mu

(defun align-by-current-symbol (&optional add-space)
  "Indent all the lines above and below the current
   by the current non-whitespace symbol."
  (interactive "P")
  (let ((symbol (current-symbol)))
    (if symbol
        (let* ((symbol. (if add-space
                            symbol
                          (concat " " symbol " ")))
               (start (or (first/last-occurance symbol. 'search-backward 'previous-line 'line-beginning-position)
                          (line-beginning-position)))
               (end (or (first/last-occurance symbol. 'search-forward-regexp 'next-line 'line-end-position)
                        (line-end-position))))
          (align-string start end (regexp-opt (list symbol.)) (point-min))))))

(defun first/last-occurance (string search move-line line-position)
  "Find the first/last line with an occurance of a string
   in a sequence of lines containing the string."
  (setq pos nil)
  (setq first nil)
  (setq try t)
  (save-excursion
    (goto-char (funcall line-position))
    (while (or try (not (equal pos nil)))
      (setq try nil)
      (setq first pos)
      (setq pos (funcall search
                         string
                         (save-excursion (funcall move-line) (funcall line-position))
                         t)))
    (if first (line-beginning-position))))

(defun current-symbol ()
  "Get the (non-whitespace) symbol at the cursor."
  (save-excursion
    (skip-chars-forward " \t")
    (let ((start (search-backward-regexp " " nil t)))
      (if start
          (let ((start. (+ 1 start)))
            (forward-char)
            (let ((end (search-forward-regexp " " nil t)))
              (if end
                  (progn
                    (backward-char)
                    (let ((str (buffer-substring-no-properties start. (point))))
                      (if (not (string-match "[\\r\\n]" str))
                          str))))))))))


;;; align-string.el --- align string components over several lines

;; Copyright (c) 2001 Markus Bjartveit Kr�ger

;; Author:   Markus Bjartveit Kr�ger <markusk@pvv.org>
;; Created:  20-Sep-2001
;; Version:  0.1
;; Keywords: convenience
;; X-URL:    http://www.pvv.org/~markusk/align-string.el

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.  This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.  See the
;; GNU General Public License for more details.  You should have
;; received a copy of the GNU General Public License along with GNU
;; Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(defun align-string (begin end regexp count)
  "Align first occurrence of REGEXP in each line of region.
If given a prefix argument, align occurrence number COUNT on each line."
  (interactive "r
sAlign by:
p")
  (save-excursion
    ;; Move begin point to start of line.
    (goto-char begin)
    (setq begin (line-beginning-position))
    ;; Make end a marker, to track updates made in buffer.  Point the
    ;; marker at the end of the last line.
    (goto-char end)
    (setq end (set-marker (make-marker) (line-end-position)))
    (let ((max-col 0))
      ;; Find max column of first occurrence of string in the lines
      ;; bounded by begin-marker and end-marker
      (goto-char begin)
      (while (< (point) end)
	(when (re-search-forward regexp (line-end-position) t count)
	  (goto-char (match-beginning 0))
	  (setq max-col (max (current-column) max-col)))
	(beginning-of-line 2))
      ;; For each line in region, indent first occurrence of string
      ;; to max column.
      (goto-char begin)
      (while (< (point) end)
	(when (re-search-forward regexp (line-end-position) t count)
	  (goto-char (match-beginning 0))
	  (indent-to max-col))
	(beginning-of-line 2)))
    ;; Clear end marker.
    (set-marker end nil)))

(defun align-all-strings (begin end regexp)
  "Align all occurrences of REGEXP in each line of region.
That is to say, align the first occurrence of each line with each other,
align the second occurence of each line with each other, and so on."
  (interactive "r
sAlign by: ")
  (save-excursion
    ;; Move begin point to start of line.
    (goto-char begin)
    (setq begin (line-beginning-position))
    ;; Make end a marker, to track updates made in buffer.  Point the
    ;; marker at the end of the last line.
    (goto-char end)
    (setq end (set-marker (make-marker) (line-end-position)))
    ;; Starting with i = 1, check if there is at least one line in
    ;; region that has at least i occurrences of regexp.  If so, align
    ;; i'th occurrence with align-string.  Otherwise, terminate.
    (let ((i 1)
	  (i-occurrences-p t))
      (while i-occurrences-p
	;; Check that at least one line in region has i occurrences
	(setq i-occurrences-p nil)
	(goto-char begin)
	(while (and (< (point) end)
		    (not i-occurrences-p))
	   (when (re-search-forward regexp (line-end-position) t i)
	     (setq i-occurrences-p t))
	   (beginning-of-line 2))
	;; Perform alignment if neccessary.
	(when i-occurrences-p
	  (align-string begin end regexp i)
	  (setq i (1+ i)))))
    ;; Clear end marker.
    (set-marker end nil)))

;;; align-string.el ends here
(provide 'align-by-current-symbol)
