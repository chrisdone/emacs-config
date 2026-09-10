;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ty case experiment

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile-match experiment

(defmacro compile-match (ps e f)
  (let* ((label. (gensym "label"))
         (e. (gensym "expr")))
    `(cl-block ,label.
       (let ((,e. ,e))
         ,@(mapcar (lambda (p-and-k)
                     (let ((p (car p-and-k))
                           (k (cdr p-and-k)))
                       (compile-worker p e. `(cl-return-from ,label. ,k))))
                   ps))
       ,f)))

(defun compile-worker (p e k)
  (if (and (consp p) (eq (car p) 'cons))
      `(when (consp ,e)
         ,(compile-worker
           (cadr p) `(car ,e)
           (compile-worker (caddr p) `(cdr ,e)
                           k)
           ))
    (if (and (consp p) (keywordp (car p)))
        `(when (and (consp ,e) (eq (car ,e) ,(car p)))
           ,(compile-worker (cdr p) `(cdr ,e) k))
      (if (consp p)
          `(when (consp ,e)
             (let ((,(car p) (car ,e)))
               ,(compile-worker (cdr p) `(cdr ,e) k)))
        (if (null p)
            k
          (error "Invalid pattern."))))))

(let ((e '((:foo 4 6) . (:baz 4))))
  (compile-match
   `(((cons (:foo p1 px) (:bar p2)) . (+ p1 p2))
     ((cons (:foo px p2) (:baz p2)) . (* p1 p2)))
   e
   'nope))
