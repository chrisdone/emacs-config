(defconst do-date-rx
  (rx
   (and word-start
         (or
          ;; today/tomorrow
          (and (group-n 1 "tom") (optional "orrow"))
          (or (and (group-n 1 "tod") (optional "ay")))
          ;; next <thing>
          (and "next "
               (group-n 2 (or "day" "week" "month" "year")))
          ;; in x <things>
          (and "in "
               (group-n 3 (or "a" (one-or-more digit)))
               " "
               (group-n 4 (or "day" "week" "month" "year"))
               (optional "s")))
         word-end)))

(defconst do-priority-rx
  (rx
   (and word-start
        (group (or "p0" "p1" "p2" "p3"))
        word-end)))
