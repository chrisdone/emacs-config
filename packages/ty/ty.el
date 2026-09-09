(defmacro ty-case (expr &rest alts)
  "Provide a basic pattern-matching utility based on a familiar sum type encoding.
Supports OTHERWISE form as in CASE."
  (declare (indent 1))
  (let* ((sym (gensym "expr"))
         ;; Any OTHERWISE alts?
         (wildcard-p
          (consp (remove-if-not
                  (lambda (alt) (eq (car alt) 'otherwise))
                  alts)))
         ;; If a wildcard is specified, then we use CASE. If no
         ;; wildcard is specified, we assume exhaustivity, and use
         ;; ECASE (which throws when no match is found, and does not
         ;; support OTHERWISE as a form).
         (case-special-form (if wildcard-p 'case 'ecase)))
    `(let ((,sym ,expr))
       (,case-special-form
        (car ,sym)
        ,@(mapcar (lambda (alt)
                    (if (eq (car alt) 'otherwise)
                        `(otherwise ,@(cdr alt))
                      `(,(caar alt)
                        (let (,@(seq-map-indexed
                                 (lambda (var i)
                                   `(,var (nth (1+ ,i) ,sym)))
                                 (cdar alt)))
                          ,@(cdr alt)))))
                  alts)))))

(defun term-show (term)
  (ty-case term
    ((:var v) (format "v%d" v))
    ((:con c) (format "%s" c))
    ((:app f x) (format "(%s %s)" (term-show f) (term-show x)))))

(term-show '(:app (:con "Maybe") (:var 1)))
