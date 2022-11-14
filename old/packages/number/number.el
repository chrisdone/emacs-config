;;; number.el --- Working with numbers at point.

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

;;; Commentary:

;; Do trivial arithmetic on the numbers at point. Attempts to preserve
;; padding when it can. Examples:

;; M-x number/add 1 RET
;; 1 -> 2
;; 05 -> 06
;; 6.30 -> 7.30
;; 07.30 -> 08.30
;; -08.30 -> -07.30

;; M-x number/pad 2 RET
;; 5 -> 05

;; M-x number/pad 2 RET 6 RET
;; 3.141 -> 03.141000

;; The "guessing" where the number is isn't yet quite awesome, e.g. it
;; doesn't know that the 05 in "2014-05-01" is a month and not,
;; e.g. the number -05. But you can use the region to explicitly
;; denote the start and end of the number.

;; The following keybindings might be nice to use:
;;
;; (global-set-key (kbd "C-c C-+") 'number/add)
;; (global-set-key (kbd "C-c C--") 'number/sub)
;; (global-set-key (kbd "C-c C-*") 'number/multiply)
;; (global-set-key (kbd "C-c C-/") 'number/divide)
;; (global-set-key (kbd "C-c C-0") 'number/pad)
;; (global-set-key (kbd "C-c C-=") 'number/eval)

;;; Code:

(defun number/add (n)
  "Add to the number at point."
  (interactive (list (number-read-from-minibuffer)))
  (number-arith-op n '+))

(defun number/sub (n)
  "Subtract to the number at point."
  (interactive (list (number-read-from-minibuffer)))
  (number-arith-op n '-))

(defun number/multiply (n)
  "Multiply the number at point."
  (interactive (list (number-read-from-minibuffer)))
  (number-arith-op n '*))

(defun number/divide (n)
  "Divide the number at point."
  (interactive (list (number-read-from-minibuffer)))
  (number-arith-op n '/))

(defun number/eval ()
  (interactive)
  (number-transform
   (lambda (number)
     (number-modify
      :number
      (lambda (x)
        (funcall (eval
                  `(lambda (n)
                     ,(read-from-minibuffer "Eval (e.g. (* n 2)): " "" nil t)))
                 x))
      number))))

(defun number/pad ()
  "Pad the number at point."
  (interactive)
  (number-transform
   (lambda (number)
     (ecase (number-get number :type)
       (integral
        (number-modify
         :padding
         (lambda (p)
           (number-read-padding
            number
            :padding
            "Pad (default: %d): "))
         number))
       (decimal
        (number-modify
         :decimal-padding
         (lambda (p)
           (number-read-padding
            number
            :decimal-padding
            "Decimal precision (default: %d): "))
         (number-modify
          :padding
          (lambda (p)
            (number-read-padding
             number
             :padding
             "Pad (default: %d): "))
          number)))))))

(defun number-read-padding (number key caption)
  "Read a padding value for a number."
  (or (let ((str (read-from-minibuffer
                  (format caption
                          (number-get number key)))))
        (unless (string= "" str)
          (string-to-number str)))
      (number-get number key)))

(defun number/mark ()
  "Mark the number at point."
  (interactive)
  (when (looking-back "[0-9.]+")
    (skip-chars-backward "[0-9.]+"))
  (when (looking-back "[+\\-]")
    (skip-chars-backward "[+\\-]"))
  (let ((point (point)))
    (skip-chars-forward "[+\\-]?[0-9.]+")
    (set-mark (point))
    (goto-char point)))

(defun number-arith-op (n op)
  "Apply the arithmetic operation to the current point."
  (number-transform
   (lambda (number)
     (number-modify
      :number
      (lambda (x) (funcall op x (number-get n :number)))
      number))))

(defun number-transform (f)
  "Transform the number at point in some way."
  (let ((point (point)))
    (let* ((beg-end (progn (unless (region-active-p)
                             (number/mark))
                           (list (region-beginning)
                                 (region-end))))
           (string (apply 'buffer-substring-no-properties beg-end)))
      (let ((new (number-format (funcall f (number-read string)))))
        (apply 'delete-region beg-end)
        (insert new)))
    (goto-char point)))

(defun number-modify (key f number)
  "Modify the number contained in a number specifier."
  (mapcar (lambda (entry)
            (if (eq (car entry) key)
                (cons key (funcall f (cdr entry)))
              entry))
          number))

(defun number-format (number)
  "Format the given number specifier to a string."
  (ecase (number-get number :type)
    (decimal
     (if (= 0 (number-get number :padding))
         (format (format "%%.%df" (number-get number :decimal-padding))
                 (number-get number :number))
       (number-pad-decimal (number-get number :padding)
                           (number-get number :decimal-padding)
                           (number-get number :number))))
    (integral
     (format (format "%%0.%dd" (number-get number :padding))
             (number-get number :number)))))

(defun number-pad-decimal (left-pad right-pad n)
  "Pad a decimal on the left- and right-hand side of the decimal
place."
  (let ((precision right-pad)
        (total (+ left-pad 1 right-pad (if (< n 0) 1 0))))
    (format (format "%%0%d.%df"
                    total
                    precision)
            n)))

(defun number-get (number key)
  "Get the KEY value from NUMBER."
  (cdr (assoc key number)))

(defun number-read-from-minibuffer ()
  "Read a number from the minibuffer."
  (number-read (read-from-minibuffer "Number: ")))

(defun number-read (string)
  "Read a number from a string."
  (cond
   ((string-match "\\." string)
    `((:string . ,string)
      (:number . ,(string-to-number string))
      (:type . decimal)
      (:padding . ,(number-padding string))
      (:decimal-padding . ,(number-decimal-padding string))))
   ((string-match "[-+]?[0-9]+" string)
    `((:string . ,string)
      (:number . ,(string-to-number string))
      (:type . integral)
      (:padding . ,(number-padding string))))
   (t (error "Unable to parse a number."))))

(defun number-padding (string)
  "Calculate the padding a number has."
  (if (string-match "[-+]?\\(\\(0+\\)[^\\.]*\\)" string)
      (length (match-string 1 string))
    0))

(defun number-decimal-padding (string)
  "Calculate the padding a number has."
  (if (string-match "\\.\\([0-9]+\\)$" string)
      (length (match-string 1 string))
    0))

(defun number-guess-padding (string)
  "Guess the padding a number has."
  (if (string-match "[-+]?\\(\\([0-9]+\\)[^\\.]*\\)" string)
      (length (match-string 1 string))
    0))

(provide 'number)
