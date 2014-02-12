(defvar tz-time-regex "\\([0-9]\\):\\([0-9]+\\)")

(defun tz-time-around-point ()
  "Get the time, and optional time zone, at point."
  (save-excursion
    (cond
     ((looking-at tz-time-regex)
      (when (looking-back "[0-9]+")
        (search-backward-regexp "[0-9]+" (line-beginning-position) t 1))
      (tz-time-at-point))
     ((or (search-backward-regexp "[0-9]+:" (line-beginning-position) t 1)
          (search-backward-regexp "[0-9]+" (line-beginning-position) t 1))
      (forward-char -1)
      (tz-time-at-point)))))

(defun tz-time-at-point ()
  "Time at point."
  (let ((string (buffer-substring-no-properties (point)
                                                (+ (point) 5))))
    (string-match tz-time-regex string)
    (cons
     (vector (string-to-number (match-string 1 string))
             (string-to-number (match-string 2 string))
             0)
     (save-excursion
       (goto-char (+ (point) 5))
       (looking-at "[ \n]*[A-Z]+")
       (buffer-substring-no-properties
        (search-forward-regexp "[ \n]+" nil nil 1)
        (search-forward-regexp "[A-Z]+" nil nil 1))))))

(provide 'tz)
