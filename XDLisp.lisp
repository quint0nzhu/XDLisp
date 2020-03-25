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

(defun value (x)
  (car x))

(defun left-subtree (x)
  (cadr x))

(defun right-subtree (x)
  (caddr x))

(defun grow-tree (x tree)
  (cond ((null tree) (make-tree x nil nil))
        ((= x (value tree)) tree)
        ((< x (value tree))
         (make-tree (value tree)
                    (grow-tree x (left-subtree tree))
                    (right-subtree tree)))
        (t (make-tree (value tree)
                      (left-subtree tree)
                      (grow-tree x (right-subtree tree))))))

(defun make-tree (value left right)
  (list value left right))

(defun search-tree (x tree)
  (cond ((null tree) nil)
        ((= x (value tree)) (cdr tree))
        ((< x (value tree))
         (search-tree x (left-subtree tree)))
        ((> x (value tree))
         (search-tree x (right-subtree tree)))))

(defun leaf-nodep (tree)
  (atom tree))

(defun left-branch (tree)
  (car tree))

(defun right-branch (tree)
  (cdr tree))

(defun enumerate-tree (tree)
  (cond
    ((null tree) nil)
    ((leaf-nodep tree)
     (cons tree nil))
    (t (append-streams (enumerate-tree (left-branch tree))
                       (enumerate-tree (right-branch tree))))))

(defun append-streams (s1 s2)
  (if (null s1)
      s2
      (cons (car s1)
            (append-streams (cdr s1) s2))))

(defun filter-odd (s)
  (cond ((null s) nil)
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

(defun accumulate-+ (s)
  (if (null s)
      0
      (+ (car s) (accumulate-+ (cdr s)))))

(defun sum-odd-square (tree)
  (accumulate-+ (map-square (filter-odd (enumerate-tree tree)))))

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

(defstruct board
  squares
  player)

(defvar *board-width* 3 "Width of a tic-tac-toe board")

(defvar *look-ahead* 3 "Number of plies to look ahead")

(defvar *min-player* 'O "The minimizing player")

(defvar *max-player* 'X "The maximizing player")

(defmacro iterate (name bindngs &body body)
  `(labels ((,name ,(mapcar #'first bindngs)
              ,@body))
     (,name ,@(mapcar #'second bindngs))))

(defmacro select (selector-form &rest clauses)
  (let ((selector (gensym "SELECTOR-")))
    `(let ((,selector ,selector-form))
       (cond ,@(mapcar #'(lambda (clause)
                           `((equal ,(car clause) ,selector)
                             ,@(cdr clause)))
                       clauses)))))

(defun opponent (player)
  (select player
          (*min-player* *max-player*)
          (*max-player* *min-player*)
          (nil nil)))

(defun rank-board (board)
  (funcall (if (eq (board-player board) *min-player*)
               #'+
               #'-)
           (apply #'+ (mapcar #'(lambda (sequence)
                                  (rank-possible-win
                                   board
                                   sequence))
                              (possible-tic-tac-toe-wins *board-width*)
                              ))))

(defun enumerate (start increment count)
  (do ((n start (+ n increment))
       (l nil (cons n l))
       (i count (1- i)))
      ((= i 0) (nreverse l))))

(defun possible-tic-tac-toe-wins (*board-width*)
  (append
   (mapcar #'(lambda(n)
               (enumerate n 1 *board-width*))
           (enumerate 0 *board-width* *board-width*))
   (mapcar #'(lambda(n)
               (enumerate n *board-width* *board-width*))
           (enumerate 0 1 *board-width*))
   (list (enumerate 0 (1+ *board-width*) *board-width*)
         (enumerate (1- *board-width*)
                    (1- *board-width*)
                    *board-width*))))

(defun rank-possible-win (board squares)
  (iterate loopy ((rest squares)
                  (number-of-occupied-squares 0)
                  (no-opponents? t))
    (if (null rest)
        (if no-opponents?
            (if (= number-of-occupied-squares *board-width*)
                (winning-score board)
                number-of-occupied-squares)
            0)
        (let ((square (svref (board-squares board)
                             (car rest))))
          (loopy (cdr rest)
                 (+ number-of-occupied-squares
                 (if (eq square
                     (opponent (board-player board)))
                 1
                 0))
                 (and no-opponents?
                      (not (eq square (board-player board)))))))))

(defun make-leaf (node)
  (make-tree node nil))

(defun tree-parent (tree)
  (car tree))

(defun tree-children (tree)
  (cdr tree))

(defun leafp (tree)
  (null (tree-children tree)))

(defun grow-tree (board depth)
  (if (= depth 0)
      (make-leaf board)
      (make-tree board
                 (mapcar #'(lambda (child)
                             (grow-tree child (1- depth)))
                         (adjacent-boards board)))))

(defun adjacent-boards (board)
  (if (terminal-board-p board)
      nil
      (do ((i 0 (1+ i))
           (moves nil (if (null (svref (board-squares board) i))
                          (cons (make-move board i) moves) moves)))
          ((= i (length (board-squares board))) moves))))

(defun make-move (from-board square)
  (let ((new-squares (make-array (length (board-squares from-board)))))
    (dotimes (i (length new-squares))
      (setf (svref new-squares i) (svref (board-squares from-board) i)))
      (setf (svref new-squares square) (board-player from-board))
      (make-board :squares new-squares
                  :player (opponent (board-player from-board)))))

(defun minimax (tree)
  (if (leafp tree)
      (values (rank-board (tree-parent tree))
              (tree-parent tree))
      (let* ((child-ranks (mapcar #'(lambda (subtree)
                                     (minimax subtree))
                                 (tree-children tree)))
        (best-rank (apply (if (eq (board-player (tree-parent tree))
                                  *min-player*)
                              #'min
                              #'max)
                          child-ranks)))
      (values best-rank
              (tree-parent (nth (position best-rank child-ranks)
                                (tree-children tree)))))))

(defun winning-score (board)
  (length (board-squares board)))

(defun terminal-board-p (board)
  (or (winning-board-p board)
      (if (position nil (board-squares board))
          nil
          :tie)))

(defun winning-board-p (board)
  (dolist (possible-win (possible-tic-tac-toe-wins *board-width*))
    (iterate loopy ((rest (cdr possible-win))
                    (player (svref (board-squares board)
                                   (car possible-win))))
             (if (null rest)
                 (unless (null player)
                   (return-from winning-board-p player))
                 (loopy (cdr rest)
                        (if (eq player (svref (board-squares
                                               board)
                                              (car rest)))
                            player
                            nil))))))

(defun make-tree (root children)
  (cons root children))

(defun ttt (&optional (first-player *max-player*))
  (format t
          "~& The computer will play ~A, You will play ~A.~%"
          *min-player*
          *max-player*)
  (let ((initial-board (make-board
                        :squares (make-array
                                  (expt *board-width* 2) :initial-element nil)
                        :player first-player)))
    (iterate loopy ((board initial-board))
             (print-board board)
      (let ((board-status (terminal-board-p board)))
               (select board-status
                       (*min-player*
                        (format t "The computer wins. ~%"))
                       (*max-player*
                        (format t "You win. ~%"))
                       (:tie
                        (format t "The game is draw."))
                       (nil
                        (loopy (if (eq (board-player board)
                                       *min-player*)
                                   (computer-move board)
                                   (human-move board)))))))))

(defun computer-move (board)
  (format t "Computer moves. Searching...")
  (let ((game-tree (grow-tree board *look-ahead*)))
    (multiple-value-bind (rank board)
        (minimax game-tree)
    (declare (ignore rank))
    (format t " done. ~%")
    board)))

(defun human-move (board)
  (format t " Your move. Please enter the number of a square: ")
  (let ((square (read)))
    (if (null (svref (board-squares board) square))
        (progn (setf (svref (board-squares board) square)
                     *max-player*)
               (setf (board-player board)
                     (opponent (board-player board)))
               board)
        (progn
          (format t " Square ~A is occupied. Try again. ~%" square)
          (human-move board)))))

(defun print-board (board)
  (labels ((divider ()
             (dotimes (i *board-width*
                         (format t "|~%"))
               (format t "|-----"))))
    (terpri)
    (divider)
    (dotimes (i (length (board-squares board)))
      (let ((square (svref (board-squares board) i)))
        (format t "|  ~A  " (if (null square) " " square)))
      (when (= (mod (1+ i) *board-width*) 0)
        (format t "|~%")
        (divider)))))

(defvar *myerrors* nil)

(defvar *mybugs* nil)

(defvar *mysave* nil)

(defvar *keytable* nil)

(defun initialise ()
  (setf *myerrors* nil)
  (setf *mybugs* nil)
  (setf *mysave* nil))

(defun execute (analysed-list)
  (cond (*myerrors* t)
        ((null analysed-list) nil)
        (t (when *mybugs* (print "EXECUTE")
                 (princ analysed-list)
                 (terpri))
           (eval analysed-list) t)
    ))

(defun analyse (input-string)
  (cond (*myerrors* t)
        ((string-equal input-string "quit") nil)
        ((string-equal input-string "") t)
        (t (syntax (reduce-token-list (scanner input-string))))))

(defun ABC ()
  (initialise)
  (princ "This is the ABC Interpreter")
  (terpri)
  (princ '|ABC> |)
  (do ()
      ((not (execute (analyse (read-line))))
       (princ "End of the ABC Interpreter") t)
    (setf *myerrors* nil)
    (princ '|ABC> |)))

(setf *keytable* '(("]" 37)
                  (("/" 56 TRUNCATE)
                   (("+" 55 +)
                    ((")" 57)
                     (("(" 54) nil nil)
                     (("*" 56 *) nil nil))
                    (("," 30) nil (("-" 55 -) nil (("->" 33) nil nil))))
                   (("put" 6)
                    (("=" 53 =)
                     ((";" 32)
                      ((":" 31) nil nil)
                      (("<" 53 <) nil nil))
                     (("if" 3)
                      (("else" 1)
                       ((">" 53 >) nil nil)
                       nil)
                      (("in" 4)
                       nil
                       (("keys" 5) nil nil))))
                    (("to" 9)
                     (("return" 7)
                      nil
                      (("select" 8) nil nil))
                     (("[" 36)
                      (("write" 11)
                       (("while" 10) nil nil)
                       nil)
                      nil))))
                  (("{}" 38 nil) nil nil)))

(defun keyentry (btree)
  (car btree))


(defun left (btree)
  (cadr btree))

(defun right (btree)
  (caddr btree))

(defun build-btree (record leftvalue rightvalue)
  (list record leftvalue rightvalue))

(defun blookup (key btree)
  (when btree
    (let ((testkey (car (keyentry btree))))
      (cond ((string-equal key testkey)
             (cdr (keyentry btree)))
            ((string-lessp key testkey)
             (blookup key (left btree)))
            (t (blookup key (right btree)))))))

(defun binsert (key value btree)
  (let ((record (if (atom value)
                    (list key value)
                    (cons key value))))
    (if (null btree)
        (build-btree record nil nil)
        (let ((testkey (car (keyentry btree))))
          (cond ((string-equal key testkey)
                 (build-btree record
                              (left btree)
                              (right
                               btree)))
                ((string-lessp key testkey)
                 (build-btree (keyentry btree)
                              (binsert key
                                       value
                                       (left btree))
                              (right btree)))
                (t (build-btree (keyentry btree)
                                (left btree)
                                (binsert key
                                         value
                                         (right btree))))
                )))))

(defun insert (key value)
  (setf *keytable* (binsert key value *keytable*)))

(defun lookup (key)
  (blookup key *keytable*))

(defun scanner (instring)
  (let* ((spacestring
           (make-sequence '(vector character)
                          10 :initial-element #\SPACE))
         (outlist nil)
         (pos2 (position #\SPACE instring))
         (pos1 0)
         (stringend (length instring))
         (newstring spacestring))
    (loop
      (unless pos2 (setf pos2 stringend))
      (setf newstring (subseq instring pos1 pos2))
      (cond ((digit-char-p (char newstring 0))
             (push (list '50 (parse-integer newstring))
                   outlist))
            ((lookup newstring)
             (push (lookup newstring) outlist))
            (t (push (list '49 newstring) outlist)))
      (when (eq pos2 stringend)
        (when *mybugs*
          (print "SCANNER") (princ outlist))
        (return outlist))
      (setf pos1 (1+ pos2)
            pos2 (position #\SPACE instring :start pos1)))))

(insert "put" 6)
(insert "if" 3)
(insert "else" 1)
(insert "in" 4)
(insert "keys" 5)
(insert "to" 9)
(insert "return" 7)
(insert "select" 8)
(insert "write" 11)
(insert "while" 10)

(defun reduce-token-list (token-list)
  (let ((reduced-list nil)
        (algebraic-list nil))
    (loop
      (cond
        (*myerrors* (return t))
        ((and (null token-list)
              (not (null algebraic-list)))
         (push (solvex algebraic-list) reduced-list)
         (return reduced-list))
        ((null token-list)
         (return reduced-list))
        (t (let ((token (pop token-list)))
             (cond
               ((and (< (car token) 58)
                     (> (car token) 49))
                (push token algebraic-list))
               ((null algebraic-list)
                (push token reduced-list))
               ((null (cdr algebraic-list))
                (push (pop algebraic-list) reduced-list)
                (push token reduced-list))
               (t (push (solvex algebraic-list) reduced-list)
                  (push token reduced-list)
                  (setf algebraic-list nil)))))))
    (when *mybugs*
      (print "Reduced-token-list")
      (princ reduced-list))
    reduced-list))

(defun tos (name)
  (when name (car name)))

(defun solvex (tlist)
  (let ((VariableStack nil)
        (OperatorStack nil))
    (when *mybugs*
      (print "SOLVEX INPUT")
      (princ tlist))
    (labels ((solve (tlist)
               (labels ((checktoken (tokenlist)
                          (let ((token (car tokenlist)))
                            (cond ((= token 50)
                                   (push (cadr tokenlist) VariableStack))
                                  ((= token 51)
                                   (push tokenlist VariableStack))
                                  ((= token 54)
                                   (push tokenlist OperatorStack))
                                  ((= token 57)
                                   (evaluate tokenlist))
                                  ((null OperatorStack)
                                   (push tokenlist OperatorStack))
                                  ((> token (car (tos OperatorStack)))
                                   (push tokenlist OperatorStack))
                                  (t (evaluate tokenlist)))))
                        (stackeval ()
                          (let ((op2 (pop VariableStack))
                                (op1 (pop VariableStack)))
                            (cond
                              ((and (numberp op2) (numberp op1)
                                    (not (equal 53 (caar OperatorStack))))
                               (push (eval (list (cadr (pop OperatorStack))
                                                 op1
                                                 op2))
                                     VariableStack))
                              (t (push (list (cadr (pop OperatorStack))
                                             op1
                                             op2)
                                       VariableStack)))))
                        (evaluate (operatortoken)
                          (if (null OperatorStack)
                              (push operatortoken OperatorStack)
                              (let ((token (car operatortoken))
                                    (stacktoken (car (tos OperatorStack))))
                                (cond
                                  ((and (= stacktoken 54)
                                        (= token 57))
                                   (pop OperatorStack))
                                  ((= token 57)
                                   (stackeval)
                                   (evaluate operatortoken))
                                  ((> token stacktoken)
                                   (push operatortoken OperatorStack))
                                  (t (stackeval)
                                     (evaluate operatortoken))))))
                        (emptystack ()
                          (do ()
                              ((null OperatorStack) nil)
                            (stackeval))))
                 (cond ((null tlist) (emptystack))
                       (t (checktoken (car tlist))
                          (solve (cdr tlist)))))))
      (solve tlist))
    (when *mybugs*
      (print "SOLVEX OUTPUT")
      (princ VariableStack))
    (cond ((numberp (car VariableStack))
           (list '50 (car VariableStack)))
          (t (list '52 (car VariableStack))))))

(defun flatten (KTB)
  (cond ((null KTB) nil)
        ((null (left KTB))
         (append (car KTB) (flatten (right KTB))))
        ((null (right KTB))
         (append (flatten (left KTB)) (car KTB)))
        (t (append (flatten (left KTB))
                   (car KTB)
                   (flatten (right KTB))))))

(defun VarSub (a)
  (when *mybugs* (print "VarSub entry") (princ a))
  (cond ((null a) nil)
        ((and (numberp (car a)) (= 50 (car a)))
         (cadr a))
        ((and (numberp (car a)) (= 51 (car a)))
         (cadr a))
        ((and (numberp (car a)) (= 52 (car a)))
         (VarSub1 (cadr a)))
        (t (princ "Error in algebraic expression")
           (terpri)
           t)))

(defun VarSub1 (a)
  (cond ((null a) nil)
        ((and (numberp (car a))
              (= 51 (car a))
              (symbolp (cadr a))
              (not (null (cadr a))))
         (cadr a))
         ((atom (car a)) (cons (car a)
                               (VarSub1 (cdr a))))
         (t (cons (VarSub1 (car a))
                  (VarSub1 (cdr a))))))

(defun search1 (token tokenlist)
  (do ((n 0 (1+ n)))
      ((or (null tokenlist)
           (equal token (car tokenlist)))
       (if (null tokenlist) nil n))
    (pop tokenlist)))

(defun search2 (token tokenlist)
  (cond ((null tokenlist) nil)
        ((equal token (car tokenlist)) t)
        ((atom (car tokenlist))
         (search2 token (cdr tokenlist)))
        (t (or (search2 token (car tokenlist))
               (search2 token (cdr tokenlist))))))

(defun set-numvar (variablename value)
  (setf *keytable*
        (insert variablename
                (list '51
                      (intern variablename)
                      variablename)))
  (set (cadr (lookup variablename))
       value))

(defun VarName (source)
  (cadr source))

(defun putfn (a)
  (let ((inpos+ (1+ (search1 '(4) a))))
    (labels ((putfn2 (a)
               (cond ((null a) nil)
                     ((= 4 (caar a)) nil)
                     ((= 30 (caar a))
                      (progn (pop a)
                             (putfn2 a)))
                     (t (cons (VarName (nth inpos+ a))
                              (cons (VarSub (pop a))
                                    (putfn2 a)))))))
      (cond ((= 51 (car (nth inpos+ a)))
             (if (search1 '(33) a)
                 (list 'progn
                       (cons 'psetf (putfn2 a))
                       '(33))
                 (cons 'psetf (putfn2 a))))
            ((= 48 (car (nth inpos+ a)))
             (insert (caddr (nth inpos+ a))
                     (cons '48
                           (cons (binsert
                                  (cadr (nth (+ 2 inpos+)
                                             a))
                                  (tos a)
                                  (cadr (nth inpos+ a)))
                                 (cddr (nth inpos+ a)))))
             t)
            ((= 49 (car (nth inpos+ a)))
             (cond ((= '38 (caar a))
                    (insert
                     (cadr (nth inpos+ a))
                     (cons '48
                           (cons nil
                                 (cdr (nth inpos+ a)))))
                    t)
                   (t (do ((tmp a (cdr tmp)))
                          ((= 4 (caar tmp)) t)
                        (unless (= 30 (caar tmp))
                          (set-numvar
                           (cadr (nth inpos+ tmp))
                           (cadar tmp)))))))
            (t (princ "Error in variable type")
               (terpri)
               (setf *myerrors* t))))))

(defun leave-keys (tablevar)
  (do ((newvar nil
               (prog1
                   (push (pop tablevar) newvar)
                 (pop tablevar)
                 (pop tablevar))))
      ((null tablevar) (reverse newvar))))

(defun writefn (a)
  (let ((writelist nil))
    (loop
      (unless a (return (append
                         (cons 'progn (reverse writelist))
                         '((terpri)))))
      (let* ((tokenlist (pop a))
             (token (car tokenlist)))
        (cond ((= 33 token)
               (push '(33) writelist))
              ((= 30 token)
               t)
              ((or (= 50 token)
                   (= 51 token))
               (push (list 'format
                           't
                           '"~8@A"
                           (cadr tokenlist))
                     writelist))
              ((= 52 token)
               (push (list 'format
                           't
                           '"~8@A"
                           (VarSub tokenlist))
                     writelist))
              ((= 48 token)
               (cond ((null a)
                      (push (list
                             'princ
                             (list
                              'quote
                              (remove
                               '50
                               (flatten (cadr tokenlist)))))
                            writelist))
                     ((= 36 (car (tos a)))
                      (push (list 'format
                                  't
                                  '"~8@A"
                                  (cadr
                                   (blookup
                                    (cadadr a)
                                    (cadr tokenlist))))
                            writelist)
                      (return (append
                               (cons 'progn
                                     (reverse writelist))
                               '((terpri)))))
                     (t (princ "Error in tale lookup")
                        (terpri)
                        (setf *myerrors* t))))
              ((= 49 token)
               (push (list 'princ (cadr tokenlist))
                     writelist))
              ((= 5 token)
               (push (list
                      'princ
                      (list 'quote
                            (leave-keys
                             (flatten (cadr (pop a))))))
                     writelist))
              ((= 32 token)
               (push '(terpri) writelist))
              (t (princ "Error in write list")
                 (terpri)
                 (setf *myerrors* t)))))))

(defun iffn (a)
  (list 'if
        (VarSub (pop a))
        (progn (pop a) (syntax1 a))))

(defun whilefn (a)
  (when *mybugs* (print "whilefn") (princ a))
  (list 'loop
        (list 'if
              (VarSub (pop a))
              (progn (pop a) (syntax1 a))
              '(return))))

(defun syntax (a)
  (let ((temp nil))
    (when *mybugs*
      (print "Syntax")
      (princ a))
    (setf temp (syntax1 a))
    (when *mybugs*
      (print "End of Syntax")
      (princ temp))
    (cond ((atom temp) temp)
          (*mysave* (setf temp
                          (subst temp
                                 '(33)
                                 *mysave*
                                 :test #'equal))
                    (setf *mysave* nil)
                    temp)
          ((search2 '(33) temp)
           (setf *mysave* temp)
           t)
          (t temp))))

(defun syntax1 (a)
  (when a
    (let* ((tokenterm (pop a))
           (token (car tokenterm)))
      (cond ((= token 6) (putfn a))
            ((= token 50)
             (writefn (push tokenterm a)))
            ((= token 11)
             (writefn a))
            ((= token 3) (iffn a))
            ((= token 10) (whilefn a))
            (t (princ "Error in syntax")
               (terpri)
               (setf *myerrors* t))))))

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
