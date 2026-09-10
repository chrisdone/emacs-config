;; Commentary
;;

(defmacro hcase (e &rest ps)
  "Supports cases of the form (HCASE E ((P RHS) (P2 RHS2)))
  where P is of the form X for anything, (A B C) for lists, (CONS P Q),
  or (:foo A B C) for a list prefixed by some keyword."
  (declare (indent 1))
  (let* ((label. (gensym "label"))
         (e. (gensym "expr")))
    `(cl-block ,label.
       (let ((,e. ,e))
         ,@(mapcar (lambda (p-and-k)
                     (let ((p (car p-and-k))
                           (k (cadr p-and-k)))
                       (hcase-compile p e. `(cl-return-from ,label. ,k))))
                   ps))
       (error "No matches for `hcase'."))))

(defun hcase-compile (p e k)
  "The work horse behind `hcase'."
  (if (and (consp p) (eq (car p) 'cons))
      `(when (consp ,e)
         ,(hcase-compile
           (cadr p) `(car ,e)
           (hcase-compile (caddr p) `(cdr ,e)
                          k)))
    (if (and (consp p) (keywordp (car p)))
        `(when (and (consp ,e) (eq (car ,e) ,(car p)))
           ,(hcase-compile (cdr p) `(cdr ,e) k))
      (if (consp p)
          `(when (consp ,e)
             (let ((,(car p) (car ,e)))
               ,(hcase-compile (cdr p) `(cdr ,e) k)))
        (if (null p)
            k
          (if (eq p 'otherwise)
              k
            (error "Invalid pattern: %S" p)))))))

(ert-deftest sanity-check-1 ()
  "Basic sanity check, first case."
  (should (= (hcase (cons (list :foo 4 6) (list :bar 4))
               ((cons (:foo p1 px) (:bar p2))
                (+ p1 p2))
               ((cons (:foo p1 p2) (:baz p2))
                (* p1 p2))
               (otherwise
                'boo))
             8)))

(ert-deftest otherwise-case ()
  "Check that otherwise works."
  (should (eq (hcase (cons (list :fob 4 6) (list :bar 4))
                ((cons (:foo p1 px) (:bar p2))
                 (+ p1 p2))
                ((cons (:foo p1 p2) (:baz p2))
                 (* p1 p2))
                (otherwise
                 'boo))
              'boo)))

(ert-deftest otherwise-list ()
  "Check that lists work."
  (should (eq (hcase (list 1 2 3)
                ((a b c)
                 'ok))
              'ok)))

(ert-deftest wildcard ()
  "Check that lists work."
  (should (equal (hcase (list 1 2 3) ((x) x)) 1)))
