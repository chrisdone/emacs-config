;; point-in-time = next <day/week/month/year>, in <duration>
;;
;; duration = n-days/week/month/year (work days)
;;
;; event-schedule =
;;   from <point-in-time> every <duration> (until <point-in-time>)?

(rx-define do-duration-rx
  (and (group-n 3 (or "a" (one-or-more digit)))
       " "
       (group-n 4 (or "day" "week" "month" "year"))
       (optional "s")))

(rx-define do-date-rx
  (and word-start
       (or
        ;; today/tomorrow
        (and (group-n 1 "tom") (optional "orrow"))
        (or (and (group-n 1 "tod") (optional "ay")))
        ;; next <thing>
        (and "next "
             (group-n 2 (or "week" "month" "year")))
        ;; in x <things>
        (and "in "
             do-duration-rx))
       word-end))

(defun do-parse-date-rx (str)
  "Parse the result of `do-date-rx' from LIST and STR into an integer of days."
  (cond
   ;; today/tomorrow
   ((string= (match-string 1 str) "tod") `(:today))
   ((string= (match-string 1 str) "tom") `(:tomorrow))
   ;; next <thing>
   ((string= (match-string 2 str) "week") `(:next-week))
   ((string= (match-string 2 str) "month") `(:next-month))
   ((string= (match-string 2 str) "year") `(:next-year))
   ;; in x <things>.
   ((string= (match-string 3 str) "a")
    `(:n-things
      . (:count 1 :thing ,(match-string 4 str))))
   ((match-string 3 str)
    `(:n-things
      . (:count ,(string-to-number (match-string 3 str)) :thing ,(match-string 4 str))))
   ))

(defconst do-priority-rx
  (rx
   (and word-start
        (group (or "p0" "p1" "p2" "p3"))
        word-end)))
