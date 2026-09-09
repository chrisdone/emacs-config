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

;; ELISP> (let((e '((:foo 4) . (:bar 4)))) (compile-match (cons (:foo p1) (:bar p2)) e (+ p1 p2) (compile-match (cons (:foo p1) (:baz p2)) e (* p1 p2) 0)))
;; 8
;;  (#o10, #x8, ?\C-h)
;; ELISP> (let((e '((:foo 4) . (:baz 4)))) (compile-match (cons (:foo p1) (:bar p2)) e (+ p1 p2) (compile-match (cons (:foo p1) (:baz p2)) e (* p1 p2) 0)))
;; 16

(defmacro compile-match (p e k f)
  (if (and (consp p) (eq (car p) 'cons))
      (let ((e. (gensym "e")))
        `(let ((,e. ,e))
           (if (consp ,e.)
               (compile-match
                ,(cadr p) (car ,e.)
                (compile-match ,(caddr p) (cdr ,e.)
                               ,k ,f)
                ,f)
             ,f)))
    (if (and (consp p) (keywordp (car p)))
        (let ((e. (gensym "e")))
          `(let ((,e. ,e))
             (if (and (consp ,e.) (eq (car ,e.) ,(car p)))
                 (compile-match ,(cdr p) (cdr ,e.) ,k ,f)
               ,f)))
        (if (consp p)
            (let ((e. (gensym "e")))
              `(let ((,e. ,e))
                 (if (consp ,e.)
                     (let ((,(car p) (car ,e.)))
                       (compile-match ,(cdr p) (cdr ,e.) ,k ,f))
                     ,f)))
            (if (null p)
                k
              (error "Invalid pattern."))))))

(defun term-show (term)
  (ty-case term
    ((:var v) (format "v%d" v))
    ((:con c) (format "%s" c))
    ((:app f x) (format "(%s %s)" (term-show f) (term-show x)))))

(term-show '(:app (:con "Maybe") (:var 1)))
