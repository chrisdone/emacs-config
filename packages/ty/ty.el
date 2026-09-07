;; a
;; (-> a b)
;; (-> (* Int Int) Int)
;; (-> (* a b) a)
;;
;;
;;

;; unify, input: constraints, output: subsitutions
;;
;; need:
;; * a 'frame' abstraction, capable of upserting, mapping and unioning
;; * a 'term' type that has holes in it
;;

(defmacro ty-ecase (expr &rest alts)
  "Provide a basic pattern-matching utility based on a familiar sum type encoding.

Example:
(defun term-show (term)
  (ty-ecase term
    ((:var v) (format \"v%d\" v))
    ((:con c) (format \"%s\" c))
    ((:app f x) (format \"(%s %s)\" (term-show f) (term-show x)))))

(term-show '(:app (:con \"Maybe\") (:var 1)))
=> (Maybe v1)"
  (declare (indent 1))
  (let ((sym (gensym "expr")))
    `(let ((,sym ,expr))
       (ecase (car ,sym)
         ,@(mapcar (lambda (alt)
                     `(,(caar alt)
                       (let (,@(seq-map-indexed
                                (lambda (var i)
                                  `(,var (nth (1+ ,i) ,sym)))
                                (cdar alt)))
                         ,@(cdr alt))))
                   alts)))))

(defmacro ty-case (expr &rest alts)
  "Provide a basic pattern-matching utility based on a familiar sum type encoding.

Example:
(defun term-show (term)
  (ty-case term
    ((:var v) (format \"v%d\" v))
    ((:con c) (format \"%s\" c))
    (otherwise \"???\")))

(term-show '(:app (:con \"Maybe\") (:var 1)))
=> ???"
  (declare (indent 1))
  (let ((sym (gensym "expr")))
    `(let ((,sym ,expr))
       (case (car ,sym)
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
  (ty-ecase term
    ((:var v) (format "v%d" v))
    ((:con c) (format "%s" c))
    ((:app f x) (format "(%s %s)" (term-show f) (term-show x)))))

(term-show '(:app (:con "Maybe") (:var 1)))
