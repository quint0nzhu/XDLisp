(defun deriv (var expr)
  (if (atom expr)
      (if (eq expr var)
          1
          0)
      (let ((operator (first expr))
            (arg1 (second expr))
            (arg2 (third expr)))
        (cond
          ((eq operator '+) (deriv-add var arg1 arg2))
          ((eq operator '*) (deriv-mult var arg1 arg2))
          ((eq operator '/) (deriv-div var arg1 arg2))
          ((eq operator 'expt) (deriv-expt var arg1 arg2))
          (t (print "unknown operator"))))))

(defun deriv-add (var arg1 arg2)
  (list '+ (deriv var arg1) (deriv var arg2)))

(defun deriv-mult (var arg1 arg2)
  (list '+ (list '* (deriv var arg1) arg2) (list '* arg1 (deriv var arg2))))

(defun deriv-div (var arg1 arg2)

  (list '/ (list '- (list '* arg2 (deriv var arg1))
                 (list '* arg1 (deriv var arg2)))
        (list 'expt arg2 2)))

(defun deriv-expt (var arg1 arg2)
  (list '* (list '* arg2 (list 'expt arg1 (list '- arg2 1)))
        (deriv var arg1)))

(defun constant? (x) (numberp x))

(defun variable? (x) (symbolp x))

(defun same-variablep (v1 v2)
  (and (variable? v1)(variable? v2)(eq v1 v2)))

(defun sump (s)
  (if (not (atom s)) (eq (car s) '+) nil))

(defun productp (s)
  (if (not (atom s)) (eq (car s) '*) nil))

(defun divisionp (s)
  (if (not (atom s)) (eq (car s) '/) nil))

(defun exponentiationp (s)
  (if (not (atom s)) (eq (car s) 'expt) nil))

(defun make-sum (a1 a2)
  (cond
    ((and (constant? a1) (constant? a2))
     (+ a1 a2))
    ((and (constant? a1) (zerop a1)) a2)
    ((and (constant? a2) (zerop a2)) a1)
    ((equal a1 a2) (list '* 2 a1))
    (t (list '+ a1 a2))))

(defun make-minus (a1 a2)
  (cond ((and (constant? a1) (constant? a2))
         (- a1 a2))
        ((and (constant? a1) (zerop a1)) (- a2))
        ((and (constant? a2) (zerop a2)) a1)
        ((equal a1 a2) 0)
        (t (list '- a1 a2))))

(defun make-product (m1 m2)
  (cond
    ((and (constant? m1) (constant? m2)) (* m1 m2))
    ((or (and (constant? m1) (zerop m1))
         (and (constant? m2) (zerop m2))) 0)
    ((and (constant? m1) (= m1 1)) m2)
    ((and (constant? m2) (= m2 1)) m1)
    (t (list '* m1 m2))))

(defun make-division (m1 m2)
  (cond
    ((and (constant? m2) (zerop m2))
     (error "divisor is zero"))
    ((and (constant? m1) (constant? m2))
     (/ m1 m2))
    ((and (constant? m1) (zerop m1)) 0)
    ((equal m1 m2) 1)
    (t (list '/ m1 m2))))

(defun make-exponentiation (b e)
  (cond
    ((and (constant? b) (constant? e))
     (expt b e))
    ((and (constant? b) (zerop b)) 1)
    ((and (constant? e) (zerop e)) 1)
    ((and (constant? b) (eql b 1)) 1)
    ((and (constant? e) (eql e 1)) b)
    (t (list 'expt b e))))

(defun addend (s)
  (cadr s))

(defun augend (s)
  (caddr s))

(defun multiplier (p)
  (cadr p))

(defun multiplicand (p)
  (and p (caddr p)))

(defun dividend (d)
  (cadr d))

(defun divisor (d)
  (caddr d))

(defun base (e) (cadr e))

(defun exponent (e) (caddr e))

(defun new-deriv (var expr)
  (cond ((constant? expr) 0)
        ((variable? expr)
         (if (same-variablep expr var) 1 0))
        ((sump expr)
         (make-sum (new-deriv var (addend expr))
                   (new-deriv var (augend expr))))
        ((productp expr)
         (make-sum
          (make-product (multiplier expr)
                        (new-deriv var (multiplicand expr)))
          (make-product (new-deriv var (multiplier expr))
                        (multiplicand expr))))
        ((divisionp expr)
         (make-division
          (make-minus (make-product
                       (divisor expr)
                       (new-deriv var (dividend expr)))
                      (make-product
                       (dividend expr)
                       (new-deriv var (divisor expr))))
          (make-exponentiation (divisor expr) 2)))
        ((exponentiationp expr)
         (make-product
          (make-product
           (exponent expr)
           (make-exponentiation (base expr)
                                (make-minus (exponent expr) 1)))
          (new-deriv var (base expr))))
        (t (error "Invalid expression"))))

(defun threat (i j a b)
  (or (equal i a)
      (equal j b)
      (equal (- i j) (- a b))
      (equal (+ i j) (+ a b))))

(defun conflict (n m board)
  (cond ((null board) nil)
        ((or (threat n m (caar board) (cadar board))
             (conflict n m (cdr board))))))

(defun queen (size)
  (prog (n m board)
     (setq n 0)
   loop-n
     (setq m 0)
   loop-m
     (cond ((conflict n m board) (go undo-m)))
     (setq board (cons (list n m) board))
     (cond ((= (setq n (1+ n)) size)
            (print (reverse board))))
     (go loop-n)
   undo-n
     (cond ((null board) (return "finish"))
           (t (setq m (cadar board)
                    n (caar board)
                    board (cdr board))))
   undo-m
     (cond ((= (setq m (1+ m)) size)
            (go undo-n))
           (t (go loop-m)))))

(defun new-queen (size)
  (queen-aux nil 0 size))

(defun queen-aux (board n size)
  (cond ((= n size) (print (reverse board)))
        (t (queen-sub board n 0 size))))

(defun queen-sub (board n m size)
  (cond ((= m size))
        (t (cond ((conflict n m board))
                 (t (queen-aux (cons (list n m) board)
                               (+ n 1)
                               size)))
           (queen-sub board n (+ m 1) size)
           )))

(set-macro-character #\?
 #'(lambda (stream char)
     (list '*var* (read stream t nil t))))

(defun match (pattern1 pattern2)
  (match-with-bindings pattern1 pattern2 nil))

(defun match-with-bindings (pattern1 pattern2 bindings)
  (cond ((pattern-var-p pattern1)
         (variable-match pattern1 pattern2 bindings))
        ((pattern-var-p pattern2)
         (variable-match pattern2 pattern1 bindings))
        ((atom pattern1)
         (if (eq pattern1 pattern2)
             (values t bindings)))
        ((atom pattern2) nil)
        (t (multiple-value-bind
                 (flag carbindings)
               (match-with-bindings
                (car pattern1)
                (car pattern2)
                bindings)
             (and flag
                  (match-with-bindings
                   (cdr pattern1)
                   (cdr pattern2)

                   carbindings))))))

(defun variable-match (pattern-var item bindings)
  (if (equal pattern-var item)
      (values t bindings)
      (let ((var-binding (get-binding pattern-var bindings)))
        (cond (var-binding
              (match-with-bindings var-binding
                                   item
                                   bindings))
              ((not (contained-in pattern-var
                                  item
                                  bindings))
               (values t (add-binding pattern-var
                                      item
                                      bindings)))))))

(defun contained-in (pattern-var item bindings)
  (cond ((atom item) nil)
        ((pattern-var-p item)
         (or (equal pattern-var item)
             (contained-in pattern-var
                           (get-binding item bindings)
                           bindings)))
        (t (or (contained-in pattern-var
                             (car item)
                             bindings)
               (contained-in pattern-var
                             (cdr item)
                             bindings)))))

(defun add-binding (pattern-var item bindings)
  (cons (list pattern-var item) bindings))

(defun pattern-var-p (item)
  (and (listp item) (eq '*var* (car item))))

(defun get-binding (pattern-var bindings)
  (cadr (assoc pattern-var bindings :test #'equal)))

(defun front-ptr (queue)
  (car queue))

(defun rear-ptr (queue)
  (cdr queue))

(defun set-front-ptr (queue item)
  (setf (car queue) item))

(defun set-rear-ptr (queue item)
  (setf (cdr queue) item))

(defun empty-queuep (queue)
  (null (front-ptr queue)))

(defun make-queue ()
  (cons '() '()))

(defun front (queue)
  (if (empty-queuep queue)
      (error "FRONT called with an empty queue ~A" queue)
      (car (front-ptr queue))))

(defun insert-queue (queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queuep queue)
           (set-front-ptr queue new-pair)
           (set-rear-ptr queue new-pair)
           queue)
          (t (setf (cdr (rear-ptr queue)) new-pair)
             (set-rear-ptr queue new-pair)
             queue))))

(defun delete-queue (queue)
  (cond ((empty-queuep queue)
         (error "DELETE called with an empty queue ~A" queue))
        (t (set-front-ptr queue (cdr (front-ptr queue)))
           queue)))

(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (1+ low) high))))

(defun map-fib (s)
  (if (null s)
      nil
      (cons (fib (car s))
            (map-fib (cdr s)))))

(defun accumulate-cons (s)
  (if (null s)
      nil
      (cons (car s) (accumulate-cons (cdr s)))))

(defun odd-fibs (n)
  (accumulate-cons
   (filter-odd
    (map-fib
     (enumerate-interval 1 n)))))

(defun fib (n)
  (fib-iter 1 1 n))

(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (1- count))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter-odd (s)
  (cond
    ((null s) nil)
    ((oddp (car s))
     (cons (car s) (filter-odd (cdr s))))
    (t (filter-odd (cdr s)))))
(defun map-square (s)
  (if (null s)
      nil
      (cons (square (car s))
            (map-square (cdr s)))))
(defun square (n)
  (expt n 2))
(defun append-streams (s1 s2)
  (if (null s1)
      s2
      (cons (car s1)
            (append-streams (cdr s1) s2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-square-fibs (n)
  (accumulate-cons
   (map-square
    (map-fib
     (enumerate-interval 1 n)))))

(defun accumulate (combiner initial-value stream)
  (if (null stream)
      initial-value
      (funcall combiner
               (car stream)
               (accumulate combiner
                           initial-value
                           (cdr stream)))))

(defun sum-stream (stream)
  (accumulate #'+ 0 stream))

(defun new-accumulate-cons (stream)
  (accumulate #'cons nil stream))

(defun flatten (stream)
  (accumulate #'append-streams nil stream))

(defun mapper (proc stream)
  (if (null stream)
      nil
      (cons (funcall proc (car stream))
            (mapper proc (cdr stream)))))

(defun filter (pred stream)
  (cond ((empty-streamp stream) (the-empty-stream))
        ((funcall pred (head stream))
         (cons-stream (head stream)
               (filter pred (tail stream))))
        (t (filter pred (tail stream)))))

(defun product-of-squares-of-odd-elements (stream)
  (accumulate #'*
              1
              (mapper #'square
                      (filter #'oddp stream))))

(defun for-each (proc stream)
  (if (null stream)
      'done
      (progn
        (print (funcall proc (car stream)))
        (for-each proc (cdr stream)))))

(defun print-stream (s)
  (for-each #'print s))

(defun sum-primes (a b)
  (labels ((iter (count accum)
             (cond
               ((> count b) accum)
               ((primep count)
                (iter (1+ count)
                      (+ count accum)))
               (t (iter (1+ count) accum)))))
    (iter a 0)))

(defun primep (n)
 (if (= n 1) nil
  (let ((temp (1+ (floor (sqrt n)))))
    (do ((i 2 (1+ i)))
        ((> i temp) t)
      (if (and (< i temp) (= 0 (mod n i))) (return nil))))))

(defun new-sum-primes (a b)
  (accumulate #'+
              0
              (filter #'primep
                      (enumerate-interval a b))))

(defmacro empty-streamp (stream)
  `(null ,stream))

(defmacro the-empty-stream ()
  nil)

(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))

(defmacro head (stream)
  `(car ,stream))

(defmacro tail (stream)
  `(force (cdr ,stream)))

(defmacro delay (exp)
  `#'(lambda() ,exp))

(defmacro force(delayed-object)
    `(funcall ,delayed-object))

(defun memo-proc (proc)
  (let ((already-run nil)
        (result nil))
    #'(lambda()
        (if (not already-run)
            (progn (setf result (funcall proc))
                   (setf already-run (not nil))
                   result)
            result))))

(defmacro new-delay (exp)
  `(memo-proc #'(lambda() ,exp)))

(defun integers-starting-from (n)
  (cons-stream n (integers-starting-from (1+ n))))

(defun sieve (stream)
  (cons-stream
   (head stream)
   (sieve (filter #'(lambda (n) (not (divisiblep n (head stream))))
                  (tail stream)))))

(defun divisiblep (n d)
  (= 0 (mod n d)))

(defun nth-stream (n s)
  (if (= n 0)
      (head s)
      (nth-stream (1- n) (tail s))))

(defun add-streams (s1 s2)
  (cond
    ((empty-streamp s1) s2)
    ((empty-streamp s2) s1)
    (t (cons-stream (+ (head s1) (head s2))
                    (add-streams (tail s1)
                                 (tail s2))))))

(defun +c (z1 z2)
  (make-rectangular (+ (real-part z1)
                       (real-part z2))
                    (+ (imag-part z1)
                       (imag-part z2))))

(defun -c (z1 z2)
  (make-rectangular (- (real-part z1)
                       (real-part z2))
                    (- (imag-part z1)
                       (imag-part z2))))

(defun *c (z1 z2)
  (make-polar (* (magnitude z1)
                 (magnitude z2))
              (+ (angle z1)
                 (angle z2))))

(defun /c (z1 z2)
  (make-polar (/ (magnitude z1)
                 (magnitude z2))
              (- (angle z1)
                 (angle z2))))

(defun make-rectangular (x y)
  (cons x y))

(defun real-part-rectangular (z)
  (car z))

(defun imag-part-rectangular (z)
  (cdr z))

(defun make-polar (r a)
  (cons (* r (cos a)) (* r (sin a))))

(defun magnitude-rectangular (z)
  (labels ((square (x)
             (* x x)))
    (sqrt (+ (square (car z)) (square (cdr z))))))

(defun angle-rectangular (z)
  (atan (cdr z) (car z)))

(defun make-rectangular (x y)
  (labels ((square (x)
             (* x x)))
    (cons (sqrt (+ (square x) (square y)))
          (atan y x))))

(defun real-part-polar (z)
  (* (car z) (cos (cdr z))))

(defun imag-part-polar (z)
  (* (car z) (sin (cdr z))))

(defun make-polar (r a)
  (cons r a))

(defun magnitude-polar (z)
  (car z))

(defun angle-polar (z)
  (cdr z))

(defun attach-type (type contents)
  (cons type contents))

(defun gtype (datum)
    (if (not (atom datum))
        (car datum)
        (error "Bad typed datum ~A-TYPE" datum)))

(defun contents (datum)
  (if (not (atom datum))
      (cdr datum)
      (error "Bad typed datum ~A-CONTENTS" datum)))

(defun rectangularp (z)
  (eq (gtype z) 'rectangular))

(defun polarp (z)
  (eq (gtype z) 'polar))

(defun make-rectangular (x y)
  (attach-type 'rectangular (cons x y)))

(defun make-polar (r a)
  (attach-type 'polar (cons r a)))

(defun real-part (z)
  (cond ((rectangularp z)
         (real-part-rectangular (contents z)))
        ((polarp z)
         (real-part-polar (contents z)))))

(defun imag-part (z)
  (cond ((rectangularp z)
         (imag-part-rectangular (contents z)))
        ((polarp z)
         (imag-part-polar (contents z)))))

(defun magnitude (z)
  (cond ((rectangularp z)
         (magnitude-rectangular (contents z)))
        ((polarp z)
         (magnitude-polar (contents z)))))

(defun angle (z)
  (cond ((rectangularp z)
         (angle-rectangular (contents z)))
        ((polarp z)
         (angle-polar (contents z)))))

(defmacro put (type op item)
  `(setf (get ,type ,op) ,item))

(put 'rectangular 'real-part 'real-part-rectangular)
(put 'rectangular 'imag-part 'imag-part-rectangular)
(put 'rectangular 'magnitude 'magnitude-rectangular)
(put 'rectangular 'angle 'angle-rectangular)

(put 'polar 'real-part 'real-part-polar)
(put 'polar 'imag-part 'imag-part-polar)
(put 'polar 'magnitude 'magnitude-polar)
(put 'polar 'angle 'angle-polar)

(defun operate (op obj)
  (let ((proc (get (gtype obj) op)))
    (if (not (null proc))
        (funcall proc (contents obj))
        (error "Operator ~A undefined for this type-OPERATE0" (list op obj)))))

(defun real-part (obj)
  (operate 'real-part obj))

(defun imag-part (obj)
  (operate 'imag-part obj))

(defun magnitude (obj)
  (operate 'magnitude obj))

(defun angle (obj)
  (operate 'angle obj))
(defstruct mymethod
  type-constraints
  handler)

(defun float-float-*/2 (x y)
  (/ (* x y) 2))

(mydefmethod */2 ((x fixnum) (y float))
  (float-float-*/2 (float x) y))

(defvar *generic-function-methods*
  (make-hash-table)
  "Hash table mapping a generic func to a list of methods")

(defmacro lookup-methods (generic-name)
  `(gethash ,generic-name *generic-function-methods*))

(defun apply-method (generic-name arg-list)
  (dolist (method (lookup-methods generic-name)
                  (error "No method found for ~A with args ~A"
                         generic-name
                         arg-list))
    (do ((args arg-list (cdr args))
         (type-constraints (mymethod-type-constraints method)
                           (cdr type-constraints)))
        ((null type-constraints)
         (return-from apply-method
           (apply (mymethod-handler method) arg-list)))
      (unless (typep (car args) (car type-constraints))
        (return)))))

(defvar *all-generic-functions*
  (make-hash-table)
  "Hash table of all known generic functions")

(defun generic-function-p (function)
  (gethash function *all-generic-functions*))

(defun ensure-generic (generic-name)
  (unless (and (fboundp generic-name)
               (generic-function-p generic-name)
               (symbol-function generic-name))
    (let ((default-method (if (fboundp generic-name)
                            (list (make-mymethod
                                   :type-constraints '()
                                   :handler
                                   (symbol-function generic-name)))
                            nil)))
    (setf (lookup-methods generic-name) default-method)
    (let ((generic-function #'(lambda (&rest args)
                                (apply-method generic-name args))))
      (setf (gethash generic-function
                     *all-generic-functions*) t)
      (setf (symbol-function generic-name)
            generic-function)))))

(defmacro mydefmethod (generic-name arg-list &body body)
  (multiple-value-bind (lambda-list type-constraints)
      (collect-lambda-list-and-types arg-list)
    `(progn (ensure-generic ',generic-name)
            (myadd-method ',generic-name
                          ',type-constraints
                          #'(lambda ,lambda-list ,@body))
            ',generic-name)))

(defun myadd-methods (generic-name type-constraints handler)
  (let ((existing-methods (lookup-methods generic-name)))
    (dolist (method existing-methods
                    (push (make-mymethod :type-constraints
                                         type-constraints
                                         :handler
                                         handler)
                          (lookup-methods generic-name)))
      (when (equal (mymethod-type-constraints method)
                   type-constraints)
        (return (setf (mymethod-handler method) handler))))))

(defun collect-lambda-list-and-types (arg-list)
  (iterate loopy ((rest arg-list)
                  (lambda-list nil)
                  (types nil))
           (let ((arg (car rest)))
             (if (or (null rest)
                     (memeber arg '(&optional &rest &key &aux)))
                 (values (append (nreverse lambda-list) rest)
                         (nreverse types))
                 (loopy (cdr rest)
                        (cons (if (atom arg)
                                  arg
                                  (first arg))
                              lambda-list)
                        (cons (if (listp arg)
                                  (second arg)
                                  t)
                              types))))))
