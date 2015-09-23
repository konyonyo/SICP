;; 2.1
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond [(and (positive? n) (positive? d)) (cons (/ n g) (/ d g))]
	  [(and (negative? n) (negative? d)) (cons (/ (abs n) g) (/ (abs d) g))]
	  [else (cons (* -1 (/ (abs n) g)) (/ (abs d) g))])))

(define (gcd a b)
  (let ((r (remainder a b)))
    (if (zero? r)
	b
	(gcd b r))))

;; 2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment segmenet)
  (car segmenet))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (begin
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")))

(define (midpoint-segment segment)
  (let ((p1 (start-segment segment))
	(p2 (end-segment segment)))
    (make-point (/ (+ (x-point p1) (x-point p2)) 2)
		(/ (+ (y-point p1) (y-point p2)) 2))))

;; 2.3
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (p1-rectangle rectangle)
  (car rectangle))

(define (p2-rectangle rectangle)
  (cdr rectangle))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (width-rectangle rectangle)
  (abs (- (x-point (p1-rectangle rectangle))
	  (x-point (p2-rectangle rectangle)))))

(define (height-rectangle rectangle)
  (abs (- (y-point (p1-rectangle rectangle))
	  (y-point (p2-rectangle rectangle)))))

(define (perimeter-rectangle rectangle)
  (+ (* 2 (width-rectangle rectangle))
     (* 2 (height-rectangle rectangle))))

(define (area-rectangle rectangle)
  (* (width-rectangle rectangle) (height-rectangle rectangle)))

;; 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; 2.5
;; 2と3は素数であるから、素因数分解の一意性より表現可能である。

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (letrec ((iter (lambda (x)
		   (if (zero? (remainder z (expt 2 x)))
		       (iter (+ x 1))
		       (- x 1)))))
    (iter 1)))

(define (cdr z)
  (letrec ((iter (lambda (x)
		   (if (zero? (remainder z (expt 3 x)))
		       (iter (+ x 1))
		       (- x 1)))))
    (iter 1)))

;; 2.6
(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; 2.7
(define (make-interval a b)
  (cons a b))

(define (upper-bound z)
  (cdr z))

(define (lower-bound z)
  (car z))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;; 2.9
;; 和
(define (add-interval-width x-width y-width)
  (/ (+ x-width y-width) 2))

;; 差
(define (sub-interval-width x-width y-width)
  (/ (+ x-width y-width) 2))

;; 乗
;; 反例 -> p1がmin, p2がmaxのとき
;;        [x1, x2] * [y1, y2] = [x1*y1, x1*y2]
;;        幅 = x1*y2 - x1*y1 = x1*(y2 - y1)
;;        これは幅だけの関数ではない。

;; 除
;; 除算は乗算を使用しているため、除算結果の幅も幅だけの関数ではない。

;; 2.10
(define (div-interval x y)
  (if (and (negative? (lower-bound y)) (positive? (upper-bound y)))
      (error "lower-bound is negative and upper-bound is positive")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))))

;; 2.11
(define (mul-interval x y)
  (let ((x1 (lower-bound x))
	(x2 (upper-bound x))
	(y1 (lower-bound y))
	(y2 (upper-bound y)))
    (cond [(and (negative? x1) (negative? x2) (negative? y1) (negative? y2))
	   (make-interval (* x1 y1) (* x2 y2))]
	  [(and (negative? x1) (negative? x2) (negative? y1) (positive? y2))
	   (make-interval (* x1 y2) (* x1 y1))]
	  [(and (negative? x1) (negative? x2) (positive? y1) (positive? y2))
	   (make-interval (* x1 y2) (* x2 y1))]
	  [(and (negative? x1) (positive? x2) (negative? y1) (negative? y2))
	   (make-interval (* x2 y1) (* x1 y1))]
	  [(and (negative? x1) (positive? x2) (negative? y1) (positive? y2))
	   (make-interval (min (* x1 y2) (* x2 y1)) (max (* x1 y1) (* x2 y2)))]
	  [(and (negative? x1) (positive? x2) (positive? y1) (positive? y2))
	   (make-interval (* x1 y2) (* x2 y2))]
	  [(and (positive? x1) (positive? x2) (negative? y1) (negative? y2))
	   (make-interval (* x2 y1) (* x1 y2))]
	  [(and (positive? x1) (positive? x2) (negative? y1) (positive? y2))
	   (make-interval (* x2 y1) (* x2 y2))]
	  [(and (positive? x1) (positive? x2) (positive? y1) (positive? y2))
	   (make-interval (* x1 y1) (* x2 y2))])))

;; 2.12
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))

;; 2.13
;; [x-dx, x+dx]*[y-dy, y+dy] = [(x-dx)*(y-dy), (x+dx)*(y+dy)]
;;                           = [x*y-(x*dy+y*dx), x*y+(x*dy+y*dx)]
;; 二つの区間の積の相対許容誤差 p = (x*dy+y*dx)/x*y

;; 2.14-2.16
(define (make-interval a b)
  (cons a b))

(define (upper-bound z)
  (cdr z))

(define (lower-bound z)
  (car z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
	(x2 (upper-bound x))
	(y1 (lower-bound y))
	(y2 (upper-bound y)))
    (cond [(and (negative? x1) (negative? x2) (negative? y1) (negative? y2))
	   (make-interval (* x1 y1) (* x2 y2))]
	  [(and (negative? x1) (negative? x2) (negative? y1) (positive? y2))
	   (make-interval (* x1 y2) (* x1 y1))]
	  [(and (negative? x1) (negative? x2) (positive? y1) (positive? y2))
	   (make-interval (* x1 y2) (* x2 y1))]
	  [(and (negative? x1) (positive? x2) (negative? y1) (negative? y2))
	   (make-interval (* x2 y1) (* x1 y1))]
	  [(and (negative? x1) (positive? x2) (negative? y1) (positive? y2))
	   (make-interval (min (* x1 y2) (* x2 y1)) (max (* x1 y1) (* x2 y2)))]
	  [(and (negative? x1) (positive? x2) (positive? y1) (positive? y2))
	   (make-interval (* x1 y2) (* x2 y2))]
	  [(and (positive? x1) (positive? x2) (negative? y1) (negative? y2))
	   (make-interval (* x2 y1) (* x1 y2))]
	  [(and (positive? x1) (positive? x2) (negative? y1) (positive? y2))
	   (make-interval (* x2 y1) (* x2 y2))]
	  [(and (positive? x1) (positive? x2) (positive? y1) (positive? y2))
	   (make-interval (* x1 y1) (* x2 y2))])))

;;(define (div-interval x y)
;;  (if (and (negative? (lower-bound y)) (positive? (upper-bound y)))
;;      (error "lower-bound is negative and upper-bound is positive")
;;      (mul-interval x (make-interval (/ 1 (upper-bound y))
;;				     (/ 1 (lower-bound y))))))

;; R/Rが1にならないことが、代数的に等価でも値が異なる理由。
;; R = [r1, r2]のとき、1/R = [1/r2, 1/r1], R*1/R = [r1/r2, r2/r1]

(define (div-interval x y)
  (let ((r1 (/ (lower-bound x) (lower-bound y)))
	(r2 (/ (lower-bound x) (upper-bound y)))
	(r3 (/ (upper-bound x) (lower-bound y)))
	(r4 (/ (upper-bound x) (upper-bound y))))
    (make-interval (min r1 r2 r3 r4) (max r1 r2 r3 r4))))
;; -> 割算をこんな風に変えてみたが、やっぱり一致しない。問題2.16はわからない。

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;; 2.17
(define (last-pair lis)
  (if (null? (cdr lis))
      lis
      (last-pair (cdr lis))))

;; 2.18
(define (reverse lis)
  (letrec ((iter (lambda (old new)
		   (if (null? old)
		       new
		       (iter (cdr old) (cons (car old) new))))))
    (iter lis '())))

;; 2.19
(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)
;; 答えは順序に依存しない。理由をうまく説明することができない。
;; このようなことを自動的に証明するプログラムが書けないだろうか?

;; 2.20
(define (same-parity a . b)
  (if (even? a)
      (filter even? (cons a b))
      (filter odd? (cons a b))))

;; 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; 2.22
;; 1. リストのcarを2乗したものをnilにconsし、その結果のリストにリストのcarを2乗したもの
;;    をconsしていくように計算しているため逆になる。

;; 2. リストをアトム(数値)にconsしているため、正しく動かない。

;; 2.23
(define (for-each f lis)
  (if (null? lis)
      #t
      (begin
	(f (car lis))
	(for-each f (cdr lis)))))

;; 2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))

;; 2.26
;; (1 2 3 4 5 6)
;; ((1 2 3) 4 5 6)
;; ((1 2 3) (4 5 6))

;; 2.27
(define (deep-reverse lis)
  (letrec ((iter (lambda (old new)
		   (cond [(null? old) new]
			 [(pair? (car old))
			  (iter (cdr old)
				(cons (iter (car old) '()) new))]
			 [else (iter (cdr old) (cons (car old) new))]))))
    (iter lis '())))

;; 2.28
(define (fringe lis)
  (cond [(null? lis) '()]
	[(pair? (car lis)) (append (fringe (car lis)) (fringe (cdr lis)))]
	[else (cons (car lis) (fringe (cdr lis)))]))

;; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

;; b
(define (total-weight mobile)
  (let ((left-structure (branch-structure (left-branch mobile)))
	(right-structure (branch-structure (right-branch mobile))))
    (cond [(and (not (pair? left-structure)) (not (pair? right-structure)))
	   (+ left-structure right-structure)]
	  [(and (not (pair? left-structure)) (pair? right-structure))
	   (+ left-structure (total-weight right-structure))]
	  [(and (pair? left-structure) (not (pair? right-structure)))
	   (+ (total-weight left-structure) right-structure)]
	  [else
	   (+ (total-weight left-structure) (total-weight right-structure))]
	  )))

;; c
(define (balanced mobile)
  (let* ((left-length (branch-length (left-branch mobile)))
	 (right-length (branch-length (right-branch mobile)))
	 (left-structure (branch-structure (left-branch mobile)))
	 (right-structure (branch-structure (right-branch mobile)))
	 (left-weight (if (not (pair? left-structure))
			  left-structure
			  (total-weight left-structure)))
	 (right-weight (if (not (pair? right-structure))
			   right-structure
			   (total-weight right-structure)))
	 (left-torque (* left-length left-weight))
	 (right-torque (* right-length right-weight)))
    (cond [(and (not (pair? left-structure)) (not (pair? right-structure)))
	   (= left-torque right-torque)]
	  [(and (not (pair? left-structure)) (pair? right-structure))
	   (and (= left-torque right-torque) (balanced right-structure))]
	  [(and (pair? left-structure) (not (pair? right-structure)))
	   (and (= left-torque right-torque) (balanced left-structure))]
	  [else
	   (and (= left-torque right-torque)
		(balanced left-structure) (balanced right-structure))])))

;; d
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)

;; 2.30
(define (square-tree tree)
  (cond [(null? tree) '()]
	[(not (pair? tree)) (square tree)]
	[else (cons (square-tree (car tree)) (square-tree (cdr tree)))]))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

;; 2.31
(define (tree-map f tree)
  (cond [(null? tree) '()]
	[(not (pair? tree)) (f tree)]
	[else (cons (tree-map f (car tree)) (tree-map f (cdr tree)))]))

;; 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* higher-terms x)))
	      0
	      coefficient-sequence))
;; Hornerの方法は多項式評価の最適アルゴリズムである。

;; 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
			 (if (not (pair? x))
			     1
			     (count-leaves x)))
		       t)))

;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
	    (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; 2.38
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (letrec ((iter (lambda (result rest)
		   (if (null? rest)
		       result
		       (iter (op result (car rest)) (cdr rest))))))
    (iter initial sequence)))

;; (fold-right / 1 (list 1 2 3)) => 3/2
;; (fold-left / 1 (list 1 2 3)) => 1/6
;; (fold-right list '() (list 1 2 3)) => (1 (2 (3 ())))
;; (fold-left list '() (list 1 2 3)) => (((() 1) 2) 3)

;; opが満たすべき性質は、(op x y) = (op y x) 可換性

;; 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;; 和が素数になるペアのリスト
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? x)
  (letrec* ((smallest-divisor (lambda (n)
				(find-divisor n 2)))
	    (find-divisor     (lambda (n test-divisor)
				(cond
				 [(> (square test-divisor) n) n]
				 [(devides? n test-divisor) test-divisor]
				 [else (find-divisor n (+ test-divisor 1))])))
	    (devides?         (lambda (a b)
				(= (remainder a b) 0))))
	   (= x (smallest-divisor x))))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (enumerate-interval i j)
  (letrec ((iter (lambda (x result)
		   (if (< x i)
		       result
		       (iter (- x 1) (cons x result))))))
    (iter j '())))

;; 集合の順列
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

;; 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (enumerate-interval i j)
  (letrec ((iter (lambda (x result)
		   (if (< x i)
		       result
		       (iter (- x 1) (cons x result))))))
    (iter j '())))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? x)
  (letrec* ((smallest-divisor (lambda (n)
				(find-divisor n 2)))
	    (find-divisor     (lambda (n test-divisor)
				(cond
				 [(> (square test-divisor) n) n]
				 [(devides? n test-divisor) test-divisor]
				 [else (find-divisor n (+ test-divisor 1))])))
	    (devides?         (lambda (a b)
				(= (remainder a b) 0))))
	   (= x (smallest-divisor x))))

;; 2.41
(define (s-sum-tuple n s)
  (filter (lambda (tuple)
	    (= s (+ (car tuple) (cadr tuple) (caddr tuple))))
          (flatmap (lambda (pair)
	             (map (lambda (i)
		            (list i (car pair) (cadr pair)))
		          (enumerate-interval 1 (- (car pair) 1))))
                   (flatmap (lambda (k)
	                      (map (lambda (j)
		                     (list j k))
		                   (enumerate-interval 1 (- k 1))))
	                    (enumerate-interval 1 n)))))

(define (enumerate-interval i j)
  (letrec ((iter (lambda (x result)
		   (if (< x i)
		       result
		       (iter (- x 1) (cons x result))))))
    (iter j '())))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

;; 2.42
(define (queens board-size)
  (letrec ((queen-cols
	    (lambda (k)
	      (if (= k 0)
		  (list empty-board)
		  (filter
		   (lambda (positions) (safe? k positions))
		   (flatmap
		    (lambda (rest-of-queens)
		      (map (lambda (new-row)
			     (adjoin-position new-row k rest-of-queens))
			   (enumerate-interval 1 board-size)))
		    (queen-cols (- k 1))))))))
    (queen-cols board-size)))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (letrec ((not-found-row (lambda (row positions)
			    (cond [(null? positions) #t]
				  [(= row (caar positions)) #f]
				  [else (not-found-row row (cdr positions))])))
	   (not-found-diag (lambda (row col positions)
			     (cond [(null? positions) #t]
				   [(or (= row (+ (caar positions)
						  (- col (cadar positions))))
					(= row (- (caar positions)
						  (- col (cadar positions)))))
				    #f]
				   [else (not-found-diag row col (cdr positions))]))))

    (if (= k 1)
          #t
          (let ((row (caar positions))
		(col k))
	    (and (not-found-row row (cdr positions))
                 (not-found-diag row col (cdr positions)))))))

(define (enumerate-interval i j)
  (letrec ((iter (lambda (x result)
		   (if (< x i)
		       result
		       (iter (- x 1) (cons x result))))))
    (iter j '())))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

;; 2.43
;; (queen-cols (- k 1))の計算が重複して実行されるため。
;; queen-colsが呼ばれる回数は以下のようになる。
;; はやい版
;; board-size = 1のとき2回
;; board-size = 2のとき3回
;; board-size = nのときn+1回
;; おそい版
;; board-size = 1のとき2回
;; board-size = 2のとき1+2*1+2*2回
;; board-size = nのとき1+n*1+n*2+...+n*n回
;; T' = O(T^3)
(define (queens board-size)
  (letrec ((queen-cols
	    (lambda (k)
	      (begin
		(display "queen-cols:")
		(display k)
		(newline)
	      (if (= k 0)
		  (list empty-board)
		  (filter
		   (lambda (positions) (safe? k positions))
		   (flatmap
		    (lambda (new-row)
		      (map (lambda (rest-of-queens)
			     (adjoin-position new-row k rest-of-queens))
			   (queen-cols (- k 1))))
		    (enumerate-interval 1 board-size))))))))
    (queen-cols board-size)))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (letrec ((not-found-row (lambda (row positions)
			    (cond [(null? positions) #t]
				  [(= row (caar positions)) #f]
				  [else (not-found-row row (cdr positions))])))
	   (not-found-diag (lambda (row col positions)
			     (cond [(null? positions) #t]
				   [(or (= row (+ (caar positions)
						  (- col (cadar positions))))
					(= row (- (caar positions)
						  (- col (cadar positions)))))
				    #f]
				   [else (not-found-diag row col (cdr positions))]))))

    (if (= k 1)
          #t
          (let ((row (caar positions))
		(col k))
	    (and (not-found-row row (cdr positions))
                 (not-found-diag row col (cdr positions)))))))

(define (enumerate-interval i j)
  (letrec ((iter (lambda (x result)
		   (if (< x i)
		       result
		       (iter (- x 1) (cons x result))))))
    (iter j '())))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

;; 2.44
(define (up-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;; 2.45
(define (split f1 f2)
  (lambda (painter n)
    (if (zero? n)
	painter
	(let ((smaller ((split f1 f2) painter (- n 1))))
	  (f1 painter (f2 smaller smaller))))))

;; 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;; 2.47
;; 1
(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (caddr frame))

;; 2
(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (cddr frame))

;; 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; 2.49
;; a
(define painter-a
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
	 (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
	 (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
	 (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0)))))

;; b
(define painter-b
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
	 (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

;; c
(define painter-c
  (segments->painter
   (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
	 (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
	 (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
	 (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))

;; d
(define painter-d
  (segments->painter
   (list (make-segment (make-vect 0.3 0.0) (make-vect 0.4 0.5))
	 (make-segment (make-vect 0.4 0.5) (make-vect 0.3 0.6))
	 (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.4))
	 (make-segment (make-vect 0.2 0.4) (make-vect 0.0 0.7))
	 (make-segment (make-vect 0.0 0.8) (make-vect 0.2 0.7))
	 (make-segment (make-vect 0.2 0.7) (make-vect 0.3 0.8))
	 (make-segment (make-vect 0.3 0.8) (make-vect 0.4 0.8))
	 (make-segment (make-vect 0.4 0.8) (make-vect 0.3 0.9))
	 (make-segment (make-vect 0.3 0.9) (make-vect 0.4 1.0))
	 (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.9))
	 (make-segment (make-vect 0.7 0.9) (make-vect 0.6 0.8))
	 (make-segment (make-vect 0.6 0.8) (make-vect 0.7 0.8))
	 (make-segment (make-vect 0.7 0.8) (make-vect 1.0 0.4))
	 (make-segment (make-vect 1.0 0.3) (make-vect 0.6 0.6))
	 (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0.0))
	 (make-segment (make-vect 0.6 0.0) (make-vect 0.5 0.3))
	 (make-segment (make-vect 0.5 0.3) (make-vect 0.4 0.0)))))

;; 2.50
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

;; 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-bottom frame)
	(paint-top frame)))))

(define (below painter1 painter2)
  (let ((painter1-rotete270 (rotate270 painter1))
	(painter2-rotate270 (rotate270 painter2)))
    (rotate180 (rotate270 (beside painter1-rotate270 painter2-rotate270)))))

;; 2.52
;; a
(define painter-d
  (segments->painter
   (list (make-segment (make-vect 0.3 0.0) (make-vect 0.4 0.5))
	 (make-segment (make-vect 0.4 0.5) (make-vect 0.3 0.6))
	 (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.4))
	 (make-segment (make-vect 0.2 0.4) (make-vect 0.0 0.7))
	 (make-segment (make-vect 0.0 0.8) (make-vect 0.2 0.7))
	 (make-segment (make-vect 0.2 0.7) (make-vect 0.3 0.8))
	 (make-segment (make-vect 0.3 0.8) (make-vect 0.4 0.8))
	 (make-segment (make-vect 0.4 0.8) (make-vect 0.3 0.9))
	 (make-segment (make-vect 0.3 0.9) (make-vect 0.4 1.0))
	 (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.9))
	 (make-segment (make-vect 0.7 0.9) (make-vect 0.6 0.8))
	 (make-segment (make-vect 0.6 0.8) (make-vect 0.7 0.8))
	 (make-segment (make-vect 0.7 0.8) (make-vect 1.0 0.4))
	 (make-segment (make-vect 1.0 0.3) (make-vect 0.6 0.6))
	 (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0.0))
	 (make-segment (make-vect 0.6 0.0) (make-vect 0.5 0.3))
	 (make-segment (make-vect 0.5 0.3) (make-vect 0.4 0.0))
	 (make-segment (make-vect 0.4 0.95) (make-vect 0.4 0.9))
	 (make-segment (make-vect 0.6 0.95) (make-vect 0.6 0.9))
	 (make-segment (make-vect 0.4 0.85) (make-vect 0.5 0.8))
	 (make-segment (make-vect 0.5 0.8) (make-vect 0.6 0.85)))))

;; b
(define (corner-split painter n)
  (if (zero? n)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1)))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter up)
		(below right corner)))))

;; c
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
				  identity  flip-horiz)))
    (combine4 (corner-split painter n))))

;; 2.53
(list 'a 'b 'c)
;; => (a b c)
(list (list 'george))
;; => ((george))
(cdr '((x1 x2) (y1 y2)))
;; => ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;; => (y1 y2)
(pair? (car '(a short list)))
;; => #f
(define (memq item x)
  (cond [(null? x) #f]
	[(eq? item (car x)) x]
	[else (memq item (cdr x))]))
(memq 'red '((red shoes) (blue socks)))
;; => #f
(memq 'red '(red shoes blue socks))
;; => (red shoes blue socks)

;; 2.54
(define (equal? a b)
  (cond [(and (not (pair? a)) (not (pair? b))) (eq? a b)]
	[(and (pair? a) (pair? b)) (and (equal? (car a) (car b))
					(equal? (cdr a) (cdr b)))]
	[else #f]))

;; 2.55
;; abracadabraは'(quote abracadabra)に等しいため。x

;; 2.56
(define (deriv exp var)
  (cond [(number? exp) 0]
	[(variable? exp) (if (same-variable? exp var) 1 0)]
	[(sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var))]
	[(product? exp) (make-sum
			 (make-product (multiplier exp)
				       (deriv (multiplicand exp) var))
			 (make-product (deriv (multiplier exp) var)
				       (multiplicand exp)))]
	[(exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product
			(make-exponentiation (base exp) (make-sum (exponent exp) -1))
			(deriv (base exp) var)))]
	[else (error "unknown expression type -- DERIV" exp)]))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
	[(=number? a2 0) a1]
	[(and (number? a1) (number? a2)) (+ a1 a2)]
	[else (list '+ a1 a2)]))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
	[(=number? m2 1) m1]
	[(and (number? m1) (number? m2)) (* m1 m2)]
	[else (list '* m1 m2)]))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-exponentiation base exponent)
  (cond [(=number? exponent 0) 1]
	[(=number? exponent 1) base]
	[else (list '** base exponent)]))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

;; 2.57
(define (deriv exp var)
  (cond [(number? exp) 0]
	[(variable? exp) (if (same-variable? exp var) 1 0)]
	[(sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var))]
	[(product? exp) (make-sum
			 (make-product (multiplier exp)
				       (deriv (multiplicand exp) var))
			 (make-product (deriv (multiplier exp) var)
				       (multiplicand exp)))]
	[(exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product
			(make-exponentiation (base exp) (make-sum (exponent exp) -1))
			(deriv (base exp) var)))]
	[else (error "unknown expression type -- DERIV" exp)]))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a b . rest)
  (letrec ((make-sum-sub (lambda (terms sum)
			   (cond [(zero? sum) (cond [(null? terms) 0]
						    [(null? (cdr terms))
						     (car terms)]
						    [else (cons '+ terms)])]
				 [(null? terms) sum]
				 [else (cons '+ (cons sum terms))]))))
    (let ((terms (filter
		   (lambda (x) (not (number? x)))
		   (cons a (cons b rest))))
	  (sum (fold + 0
		     (filter (lambda (x) (number? x))
			     (cons a (cons b rest))))))
      (make-sum-sub terms sum))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (if (null? (cdddr s))
		       (caddr s)
		       (apply make-sum (cddr s))))

(define (make-product a b . rest)
  (letrec ((make-product-sub (lambda (terms product)
			       (cond [(= product 0) 0]
				     [(= product 1) (cond [(null? terms) 1]
							  [(null? (cdr terms))
							   (car terms)]
							  [else (cons '* terms)])]
				     [(null? terms) product]
				     [else (cons '* (cons product terms))]))))
    (let ((terms (filter
		  (lambda (x) (not (number? x)))
		  (cons a (cons b rest))))
	  (product (fold * 1
			 (filter (lambda (x) (number? x))
				 (cons a (cons b rest))))))
      (make-product-sub terms product))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (if (null? (cdddr p))
			     (caddr p)
			     (apply make-product (cddr p))))

(define (make-exponentiation base exponent)
  (cond [(=number? exponent 0) 1]
	[(=number? exponent 1) base]
	[else (list '** base exponent)]))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

;; 2.58
;; a
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
	[(=number? a2 0) a1]
	[(and (number? a1) (number? a2)) (+ a1 a2)]
	[else (list a1 '+ a2)]))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
	[(=number? m1 1) m2]
	[(=number? m2 1) m1]
	[(and (number? m1) (number? m2)) (* m1 m2)]
	[else (list m1 '* m2)]))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond [(number? exp) 0]
	[(variable? exp) (if (same-variable? exp var) 1 0)]
	[(sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var))]
	[(product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp)))]
	[else
	 (error "unknown expression type -- DERIV" exp)]))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; b
(define (variable? x) (symbol? x))

(define (rm-paren x)
  (if (one-elem-list? x)
      (if (or (number? (car x)) (variable? (car x)))
	  (car x)
	  (rm-paren (car x)))
      x))

(define (one-elem-list? x)
  (and (pair? x) (null? (cdr x))))

(define (sum? x)
  (let ((item (rm-paren x)))
    (and (pair? item) (term? (previous '+ item)))))

(define (term? x)
  (if (not (pair? x))
      (or (number? x) (variable? x))
      (or (one-elem-list? x) (not (member? '+ x)))))

(define (member? item x)
  (cond [(null? x) #f]
	[(eq? item (car x)) #t]
	[else (member? item (cdr x))]))

(define (previous item x)
  (letrec ((iter (lambda (result rest)
		   (cond [(null? rest) '()]
			 [(eq? (car rest) item) (reverse result)]
			 [else (iter (cons (car rest) result) (cdr rest))]))))
    (iter '() x)))

(define (next item x)
  (cond [(null? x) '()]
	[(eq? (car x) item) (cdr x)]
	[else (next item (cdr x))]))

(define (addend s)
  (rm-paren (previous '+ (rm-paren s))))

(define (augend s)
  (rm-paren (next '+ (rm-paren s))))

(define (operators x)
  (filter (lambda (y) (or (eq? y '+) (eq? y '*))) x))

(define (product? x)
  (let ((item (rm-paren x)))
    (and (pair? item)
	 (not (null? (operators item)))
	 (eq? (car (operators item)) '*)
	 (term? (next '* item)))))

(define (multiplier p)
  (rm-paren (previous '* (rm-paren p))))

(define (multiplicand p)
  (rm-paren (next '* (rm-paren p))))

(define (deriv exp var)
  (cond [(number? exp) 0]
	[(variable? exp) (if (same-variable? exp var) 1 0)]
	[(sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var))]
	[(product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp)))]
	[else (error "unknown expression type -- DERIV" exp)]))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
	[(=number? a2 0) a1]
	[(and (number? a1) (number? a2)) (+ a1 a2)]
	[else (list a1 '+ a2)]))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
	[(=number? m1 1) m2]
	[(=number? m2 1) m1]
	[(and (number? m1) (number? m2)) (* m1 m2)]
	[else (list m1 '* m2)]))

;; 2.59
(define (union-set set1 set2)
  (cond [(null? set1) set2]
	[(null? set2) set1]
	[(element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2)]
	[else (cons (car set1) (union-set (cdr set1) set2))]))

;; 2.60
(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;; element-of-set?, intersectionは変えなくてよい。

;; 2.61
(define (adjoin-set x set)
  (cond [(null? set) (cons x set)]
	[(= (car set) x) set]
	[(< x (car set)) (cons x set)]
	[(< (car set) x) (cons (car set) (adjoin-set x (cdr set)))]))

;; 2.62
(define (union-set set1 set2)
  (cond [(null? set1) set2]
	[(null? set2) set1]
	[else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond [(= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2)))]
		      [(< x1 x2) (cons x1 (union-set (cdr set1) set2))]
		      [(< x2 x1) (cons x2 (union-set set1 (cdr set2)))]))]))

;; 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (letrec ((copy-to-list (lambda (tree result-list)
			   (if (null? tree)
			       result-list
			       (copy-to-list
				(left-branch tree)
				(cons (entry tree)
				      (copy-to-list (right-branch tree)
						    result-list)))))))
    (copy-to-list tree '())))

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define tree1 (make-tree 7
			 (make-tree 3
				    (make-tree 1 '() '())
				    (make-tree 5 '() '()))
			 (make-tree 9
				    '()
				    (make-tree 11 '() '()))))

(define tree2 (make-tree 3
			 (make-tree 1 '() '())
			 (make-tree 7
				    (make-tree 5 '() '())
				    (make-tree 9
					       '()
					       (make-tree 11 '() '())))))

(define tree3 (make-tree 5
			 (make-tree 3
				    (make-tree 1 '() '())
				    '())
			 (make-tree 9
				    (make-tree 7 '() '())
				    (make-tree 11 '() '()))))

;; a すべての木に対して同じ結果を生じる。

;; b
;; tree->list-1はO(log(n))
;; tree->list-2は、(n/2)*(n/2) -> (n/2^2)*(n/2^2) -> ... -> (n/2^x)*(n/2^x)
;; 2^x*2^x = 2^(2x) = n^2より、x = log(n)
;; すなわちステップ数の増加の程度は同じ。

;; 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
	        (non-left-elts (cdr left-result))
	        (right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree
				 (cdr non-left-elts) right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
	        (cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

;; a
;; 左部分木のサイズはfloor((n - 1)/2)。
;; 左部分木はpartial-treeに左部分木のサイズを引数として与えることにより、得られる。
;; 節はpartial-treeのcadrである。
;; 右部分木のサイズはn - (左部分木のサイズ + 1)。
;; 右部分木はpartial-treeに右部分木のサイズを引数として与えることにより、得られる。
;; 上記で評価するpartial-treeに引数として与える部分木のサイズが0になるまで、
;; 上記のプロセスが繰り返される。
;; 得られた節、左部分木、右部分木を使って木を構成する。

;; リスト(1 3 5 7 9 11)に対して作る木は
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))。

;; b
;; partial-treeの引数であるサイズは半分ずつになっていくため、O(log(n))。

;; 2.65
(define (union-set tree1 tree2)
  (letrec ((union-set-sub (lambda (set1 set2)
			    (cond [(null? set1) set2]
				  [(null? set2) set1]
				  [else (let ((x1 (car set1))
					      (x2 (car set2)))
					  (cond [(= x1 x2) (cons x1 (union-set-sub (cdr set1) (cdr set2)))]
						[(< x1 x2) (cons x1 (union-set-sub (cdr set1) set2))]
						[(< x2 x1) (cons x2 (union-set-sub set1 (cdr set2)))]))]))))
    (let ((set1 (tree->list-1 tree1))
	  (set2 (tree->list-1 tree2)))
      (union-set-sub set1 set2))))

(define (intersection-set tree1 tree2)
  (letrec ((intersection-set-sub (lambda (set1 set2)
				   (if (or (null? set1) (null? set2))
				       '()
				       (let ((x1 (car set1))
					     (x2 (car set2)))
					 (cond [(= x1 x2) (cons x1 (intersection-set-sub (cdr set1) (cdr set2)))]
					       [(< x1 x2) (intersection-set-sub (cdr set1) set2)]
					       [(< x2 x1) (intersection-set-sub set1 (cdr set2))]))))))
    (let ((set1 (tree->list-1 tree1))
	  (set2 (tree->list-1 tree2)))
      (intersection-set-sub set1 set2))))


;; 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((entry (entry set-of-records))
	    (left (left-branch set-of-records))
	    (right (right-branch set-of-records)))
	(cond [(= given-key (key entry)) entry]
	      [(< given-key (key entry))
	       (lookup given-key left)]
	      [(> given-key (key entry))
	       (lookup given-key right)]))))
;; Huffman
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (letrec ((decode-1 (lambda (bits current-branch)
		       (if (null? bits)
			   '()
			   (let ((next-branch
				  (choose-branch (car bits) current-branch)))
			     (if (leaf? next-branch)
				 (cons (symbol-leaf next-branch)
				       (decode-1 (cdr bits) tree))
				 (decode-1 (cdr bits) next-branch)))))))
    (decode-1 bits tree)))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
	[(= bit 1) (right-branch branch)]
	[else (error "bad bit -- CHOOSE-BRANCH" bit)]))

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
	[(< (weight x) (weight (car set))) (cons x set)]
	[else (cons (car set) (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; (decode sample-message sample-tree) => (A D A B B C A)

;; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (letrec ((encode-symbol-sub
            (lambda (bits tree)
              (cond [(not (member? symbol (symbols tree)))
                     (error "not found symbol -- ENCODE-SYMBOL" symbol)]
                    [(member? symbol (symbols (left-branch tree)))
                     (if (leaf? (left-branch tree))
                         (reverse (cons 0 bits))
                         (encode-symbol-sub (cons 0 bits) (left-branch tree)))]
                    [else
                     (if (leaf? (right-branch tree))
                         (reverse (cons 1 bits))
                         (encode-symbol-sub (cons 1 bits) (right-branch tree)))]))))
    (encode-symbol-sub '() tree)))

(define (member? item x)
  (cond [(null? x) #f]
        [(equal? (car x) item) #t]
        [else (member? item (cdr x))]))

;; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (if (null? (cdr trees))
      (car trees)
      (successive-merge
       (adjoin-set (make-code-tree (car trees) (cadr trees))
                   (cddr trees)))))

;; 2.70
(define rock-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define rock-tree (generate-huffman-tree rock-pairs))

(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(encode song rock-tree)
;; => (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
;; => 84bit

;; 8記号アルファベットの固定長符号を使うと、符号化するのに必要な最小ビット数は、
;; (* (length song) 3) = 108bit。

;; 2.73
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else ((get 'deriv (operator exp)) (operands exp) var)]))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a
;; 型タグであるoperatorに対応する演算derivをgetして実行する。
;; numberとvariableはpairでないため、operatorやoperandsを実行するために
;; numberとvariableに対してcar, cdrを実行するとエラーになるから。

;; b
(define (deriv-sum operands var)
  (make-sum (deriv (addend operands) var)
            (deriv (augend operands) var)))

(put 'deriv '+ deriv-sum)

(define (deriv-product operands var)
  (make-sum (make-product (multiplier operands)
                          (deriv (multiplicand operands) var))
            (make-product (deriv (multiplier operands) var)
                          (multiplicand operands))))

(put 'deriv '* deriv-product)

;; c
(define (deriv-exponentiation operands var)
  (make-product (make-product (base operands)
                              (make-exponentiation (base operands)
                                                   (- (exponent operands) 1)))
                (deriv (base operands) var)))

(put 'deriv '** deriv-exponentiation)

;; d
;; 表の行と列の入れ替えが必要。つまりput手続きを変更する必要がある。

;; 2.74
;; a
(define (get-record employee-name file)
  ((get 'get-record (office file)) employee-name file))

;; 事業所ファイルはofficeという型情報を持つ。

;; b
(define (get-salary employee-record)
  ((get 'get-salary (office employee-record)) employee-record))

;; 各従業員のレコードはofficeという型情報を持つ。

;; c
(define (find-employee-record employee-name files)
  (if (null? files)
      '()
      (if (null? (get-record employee-neme (car files)))
          (find-employee-record employee-name (cdr files))
          (get-record employee-name (car files)))))

;; d
;; 新しいofficeに対応する新しいget-record手続き、get-salary手続き、get-address手続き
;; などを定義し、
;; (put 'get-record <office> get-record-<office>)
;; (put 'get-salary <office> get-salary-<office>)
;; (put 'get-address <office> get-address-<office>)
;; というように、定義した手続きを表に登録する。

;; 2.75
(define (make-from-mag-ang r a)
  (let ((dispatch (lambda (op)
                    (cond [(eq? op 'real-part) (* r (cos a))]
                          [(eq? op 'imag-part) (* r (sin a))]
                          [(eq? op 'magnitude) r]
                          [(eq? op 'angle) a]
                          [else (error "Unknown op -- MAKE-FROM-MAG-ANG" op)]))))
    dispatch))

;; 2.76
;; 1. 施すべき変更
;;  (i) 明白な振分けを持つ汎用演算
;;    新しい型を追加する時、新しい型の構成子、選択子を追加する必要がある。
;;    新しい演算を追加する時、変更なし。
;;  (ii) データ主導流
;;    新しい型を追加する時、新しい型に対応する既存の演算を追加する必要がある。
;;    また、その演算をputする必要がある。
;;    新しい演算を追加する時、既存の型に対応する新しい演算を追加する必要がある。
;;    また、その演算をputする必要がある。
;;  (iii) メッセージパッシング流
;;    新しい型を追加する時、新しい型に対応する既存の演算を追加する必要がある。
;;    (新しい型のデータオブジェクトである関数を追加する。)
;;    新しい演算を追加する時、既存の型に対応する新しい演算を追加する必要がある。
;;    (既存の型のデータオブジェクトである関数を書き換える。)
;;
;; 2. 新しい型が絶えず追加されるシステムに最適な方法
;;  メッセージパッシング流(追加される型に対応する1つの関数を追加すればよいため。)
;;
;; 3. 新しい演算が絶えず追加されるシステムに最適な方法
;;  明白な振分けを持つ汎用演算(変更がないため。)

;; 汎用算術演算
;; 2.77
(define table '())

(define (put func type contents)
  (set! table (cons (list func type contents) table)))

(define (get func type)
  (letrec ((get-sub (lambda (func type table)
                      (cond [(null? table)
                             (error "table is null -- GET" func type)]
                            [(and (equal? func (caar table))
                                  (equal? type (cadar table)))
                             (caddar table)]
                            [else (get-sub func type (cdr table))]))))
    (get-sub func type table)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    'done))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           'done))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (r a)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2)))))
         (tag (lambda (z) (attach-tag 'complex z))))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z) (sqrt (+ (square (real-part z))
                                         (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a) (cons (* r (cos a)) (* r (sin a)))))
    (tag (lambda (x) (attach-tag 'rectangular x))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-polar-package)
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z) (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z) (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x))))
    (tag (lambda (x) (attach-tag 'polar x))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; (magnitude z)のトレース
;; (magnitude (make-complex-from-real-imag 1 2))
;; (magnitude '(complex rectangular 1 . 2))
;; (apply-generic 'magnitude '(complex rectangular 1 . 2))
;; (apply (get 'magnitude (map type-tag '((complex rectangular 1 . 2))))
;;        (map contents '((complex rectangular 1 . 2))))
;; (apply (get 'magnitude '(complex))
;;        '((rectangular 1 . 2)))
;; ((get 'magnitude '(complex)) '(rectangular 1 . 2))
;; (magnitude '(rectangular 1 . 2))
;; (apply-generic 'magnitude '(rectangular 1 . 2))
;; (apply (get 'magnitude (map type-tag '((rectangular 1 . 2))))
;;        (map contents '((rectangular 1 . 2))))
;; (apply (get 'magnitude '(rectangular))
;;        '((1 . 2)))
;; (apply (lambda (z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
;;        '((1 . 2)))
;; (sqrt (+ (square (real-part '(1 . 2)))
;;          (square (imag-part '(1 . 2)))))
;; (sqrt (+ (square (car '(1 . 2))) (square (cdr '(1 . 2)))))
;; (sqrt (+ (square 1) (square 2)))
;; apply-genericは2回呼び出される。

;; 2.78
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents))))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))


;; 2.79
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (tag (= x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    'done))

(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'equ? '(rational rational)
                (lambda (x y) (= (/ (numer x) (denom x)) (/ (numer y) (denom y)))))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           'done))

(define (install-complex-package)
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (r a)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2)))))
         (tag (lambda (z) (attach-tag 'complex z))))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'equ? '(complex complex)
         (lambda (x y) (and (= (real-part x) (real-part y))
                            (= (imag-part x) (imag-part y)))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-rectangular-package)
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z) (sqrt (+ (square (real-part z))
                                         (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a) (cons (* r (cos a)) (* r (sin a)))))
    (tag (lambda (x) (attach-tag 'rectangular x))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'equ? '(rectangular rectangular)
         (lambda (x y) (and (= (real-part x) (real-part y))
                            (= (imag-part x) (imag-part y)))))
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-polar-package)
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z) (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z) (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x))))
    (tag (lambda (x) (attach-tag 'polar x))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'equ? '(polar polar)
         (lambda (x y) (and (= (real-part x) (real-part y))
                            (= (imag-part x) (imag-part y)))))
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;; 2.80
(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (tag (= x y))))
    (put '=zero? '(scheme-number)
         (lambda (x) (= x 0)))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    'done))

(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'equ? '(rational rational)
                (lambda (x y) (= (/ (numer x) (denom x)) (/ (numer y) (denom y)))))
           (put '=zero? '(rational)
                (lambda (x) (= (numer x) 0)))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           'done))

(define (install-complex-package)
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (r a)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2)))))
         (tag (lambda (z) (attach-tag 'complex z))))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'equ? '(complex complex)
         (lambda (x y) (and (= (real-part x) (real-part y))
                            (= (imag-part x) (imag-part y)))))
    (put '=zero? '(complex)
         (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-rectangular-package)
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z) (sqrt (+ (square (real-part z))
                                         (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a) (cons (* r (cos a)) (* r (sin a)))))
    (tag (lambda (x) (attach-tag 'rectangular x))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'equ? '(rectangular rectangular)
         (lambda (x y) (and (= (real-part x) (real-part y))
                            (= (imag-part x) (imag-part y)))))
    (put '=zero? '(rectangular)
         (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-polar-package)
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z) (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z) (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x))))
    (tag (lambda (x) (attach-tag 'polar x))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'equ? '(polar polar)
         (lambda (x y) (and (= (real-part x) (real-part y))
                            (= (imag-part x) (imag-part y)))))
    (put '=zero? '(polar)
         (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;; 2.81
;; a
(define table-coercion '())

(define (put-coercion type-from type-to contents)
  (set! table-coercion
        (cons (list type-from type-to contents) table-coercion)))

(define (get-coercion type-from type-to)
  (letrec ((get-coercion-sub (lambda (type-from type-to table)
                               (cond [(null? table) #f]
                                     [(and (eq? (caar table) type-from)
                                           (eq? (cadar table) type-to))
                                      (caddar table)]
                                     [else (get-coercion-sub
                                            type-from
                                            type-to
                                            (cdr table))]))))
    (get-coercion-sub type-from type-to table-coercion)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                        [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                        [else (error "No method for these types"
                                     (list op type-tags))])))
              (error "No method for these types"
                     (list op type-tags)))))))

(define table '())

(define (put func type contents)
  (set! table (cons (list func type contents) table)))

(define (get func type)
  (letrec ((get-sub (lambda (func type table)
                      (cond [(null? table) #f]
                            [(and (equal? func (caar table))
                                  (equal? type (cadar table)))
                             (caddar table)]
                            [else (get-sub func type (cdr table))]))))
    (get-sub func type table)))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'exp '(scheme-number scheme-number)
         (lambda (x y) (tag (expt x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    'done))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           'done))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (r a)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2)))))
         (tag (lambda (z) (attach-tag 'complex z))))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z) (sqrt (+ (square (real-part z))
                                         (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a) (cons (* r (cos a)) (* r (sin a)))))
    (tag (lambda (x) (attach-tag 'rectangular x))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-polar-package)
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z) (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z) (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x))))
    (tag (lambda (x) (attach-tag 'polar x))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; 強制型変換 scheme-number->complex
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;; 強制型変換 scheme-number->scheme-number
(define (scheme-number->scheme-number n) n)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

;; 強制型変換 complex->complex
(define (complex->complex z) z)

(put-coercion 'complex 'complex complex->complex)

;; べき乗演算
(define (exp x y) (apply-generic 'exp x y))

;; (exp (make-complex-from-real-imag 3 0) (make-complex-from-real-imag 2 0))
;; => apply-generic関数で無限ループになる。

;; b
;; 同じ型の引数の強制型変換について何かすべきだというLouisは正しくない。
;; apply-generic関数で同じ型の引数の強制型変換はしないようにしないと無限ループになってしまう。

;; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                            [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                            [else (error "No method for these types"
                                         (list op type-tags))]))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 2.82
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (letrec* ((type-convert (lambda (type-tags type-to)
                                   (cond [(null? type-tags) '()]
                                         [(get-coercion (car type-tags)
                                                        type-to)
                                          (cons (get-coercion (car type-tags)
                                                              type-to)
                                                (type-convert (cdr type-tags)
                                                              type-to))]
                                         [else #f])))
                    (type-convert-iter (lambda (type-tags type-tags-org)
                                         (if (null? type-tags)
                                             (error "No method for these types"
                                                    (list op type-tags-org))
                                             (let ((type-convert-result
                                                    (type-convert
                                                     type-tags-org
                                                     (car type-tags))))
                                               (if type-convert-result
                                                   (apply apply-generic
                                                          (cons op (apply-funcs-to-list type-convert-result args)))
                                                   (type-convert-iter (cdr type-tags) type-tags-org))))))
                    (apply-funcs-to-list (lambda (func-list list)
                                           (if (null? list)
                                               '()
                                               (cons ((car func-list)
                                                      (car list))
                                                     (apply-funcs-to-list
                                                      (cdr func-list)
                                                      (cdr list)))))))
                   (type-convert-iter type-tags type-tags))))))

;; 例えば型(scheme-number complex)などの異なる型同士の演算を実行することができない。

;; 2.83
;; 汎用raise演算
(define (raise x) (apply-generic 'raise x))

;; 整数パッケージ
(define (install-integer-package)
  (let ((tag (lambda (x) (attach-tag 'integer x))))
    (put 'add '(integer integer)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(integer integer)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(integer integer)
         (lambda (x y) (tag (* x y))))
    (put 'div '(integer integer)
         (lambda (x y) (tag (quotient x y))))
    (put 'make 'integer
         (lambda (x) (tag (floor x))))
    (put 'raise '(integer)
         (lambda (x) (make-rational x 1)))
    'done))

;; 有理数パッケージ
(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           (put 'raise '(rational)
                (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
           'done))

;; 実数パッケージ
(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (put 'raise '(scheme-number)
         (lambda (x) (make-complex-from-real-imag x 0)))
    'done))

;; 2.84
;; 型の塔における高さ
(define (type-height x)
  (if (eq? (type-tag x) 'complex)
      0
      (+ 1 (type-height (raise x)))))

;; 修正したapply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond [(> (type-height a1) (type-height a2))
                       (apply-generic op (raise a1) a2)]
                      [(< (type-height a1) (type-height a2))
                       (apply-generic op a1 (raise a2))]
                      [else (error "No method for these types"
                                   (list op type-tags))]))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 汎用raise演算
(define (raise x) (apply-generic 'raise x))

;; 整数パッケージ
(define (install-integer-package)
  (let ((tag (lambda (x) (attach-tag 'integer x))))
    (put 'add '(integer integer)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(integer integer)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(integer integer)
         (lambda (x y) (tag (* x y))))
    (put 'div '(integer integer)
         (lambda (x y) (tag (quotient x y))))
    (put 'make 'integer
         (lambda (x) (tag (floor x))))
    (put 'raise '(integer)
         (lambda (x) (make-rational x 1)))
    'done))

;; 有理数パッケージ
(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           (put 'raise '(rational)
                (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
           'done))

;; 実数パッケージ
(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (put 'raise '(scheme-number)
         (lambda (x) (make-complex-from-real-imag x 0)))
    'done))

(define table '())

(define (put func type contents)
  (set! table (cons (list func type contents) table)))

(define (get func type)
  (letrec ((get-sub (lambda (func type table)
                      (cond [(null? table) #f]
                            [(and (equal? func (caar table))
                                  (equal? type (cadar table)))
                             (caddar table)]
                            [else (get-sub func type (cdr table))]))))
    (get-sub func type table)))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (r a)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2)))))
         (tag (lambda (z) (attach-tag 'complex z))))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z) (sqrt (+ (square (real-part z))
                                         (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a) (cons (* r (cos a)) (* r (sin a)))))
    (tag (lambda (x) (attach-tag 'rectangular x))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-polar-package)
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z) (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z) (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x))))
    (tag (lambda (x) (attach-tag 'polar x))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; 2.85
;; 汎用等価述語
(define (equ? x y) (apply-generic 'equ? x y))

;; 汎用project演算
(define (project x) (apply-generic 'project x))

;; drop演算
(define (drop x)
  (let ((projected (project x)))
    (if (equ? x (raise projected))
        (if (eq? (type-tag projected) 'integer)
            projected
            (drop projected))
        x)))

;; 型の塔における高さ
(define (type-height x)
  (if (eq? (type-tag x) 'complex)
      0
      (+ 1 (type-height (raise x)))))

;; 修正したapply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (or (eq? op 'project) (eq? op 'raise) (eq? op 'equ?))
              (apply proc (map contents args))
              (drop (apply proc (map contents args))))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond [(> (type-height a1) (type-height a2))
                       (apply-generic op (raise a1) a2)]
                      [(< (type-height a1) (type-height a2))
                       (apply-generic op a1 (raise a2))]
                      [else (error "No method for these types"
                                   (list op type-tags))]))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 汎用raise演算
(define (raise x) (apply-generic 'raise x))

;; 整数パッケージ
(define (install-integer-package)
  (let ((tag (lambda (x) (attach-tag 'integer x))))
    (put 'add '(integer integer)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(integer integer)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(integer integer)
         (lambda (x y) (tag (* x y))))
    (put 'div '(integer integer)
         (lambda (x y) (tag (quotient x y))))
    (put 'equ? '(integer integer)
         (lambda (x y) (= x y)))
    (put 'make 'integer
         (lambda (x) (tag (floor x))))
    (put 'project '(integer)
         (lambda (x) (tag x)))
    (put 'raise '(integer)
         (lambda (x) (make-rational x 1)))
    'done))

;; 有理数パッケージ
(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'equ? '(rational rational)
                (lambda (x y) (= (/ (numer x) (denom x))
                                 (/ (numer y) (denom y)))))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           (put 'raise '(rational)
                (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
           (put 'project '(rational)
                (lambda (x) (make-integer (numer x))))
           'done))

;; 実数パッケージ
(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (put 'raise '(scheme-number)
         (lambda (x) (make-complex-from-real-imag x 0)))
    (put 'project '(scheme-number)
         (lambda (x) (make-rational (floor x) 1)))
    'done))

(define table '())

(define (put func type contents)
  (set! table (cons (list func type contents) table)))

(define (get func type)
  (letrec ((get-sub (lambda (func type table)
                      (cond [(null? table) #f]
                            [(and (equal? func (caar table))
                                  (equal? type (cadar table)))
                             (caddar table)]
                            [else (get-sub func type (cdr table))]))))
    (get-sub func type table)))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (r a)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (+ (contents (real-part z1)) (contents (real-part z2)))
                         (+ (contents (imag-part z1)) (contents (imag-part z2))))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (- (contents (real-part z1)) (contents (real-part z2)))
                         (- (contents (imag-part z1)) (contents (imag-part z2))))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (* (contents (magnitude z1)) (contents (magnitude z2)))
                         (+ (contents (angle z1)) (contents (angle z2))))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (/ (contents (magnitude z1)) (contents (magnitude z2)))
                         (- (contents (angle z1)) (contents (angle z2))))))
         (tag (lambda (z) (attach-tag 'complex z))))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'equ? '(complex complex)
         (lambda (z1 z2) (and (= (contents (real-part z1)) (contents (real-part z2)))
                              (= (contents (imag-part z1)) (contents (imag-part z2))))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'project '(complex)
         (lambda (z) (make-scheme-number (contents (real-part z)))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z) (sqrt (+ (square (real-part z))
                                         (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a) (cons (* r (cos a)) (* r (sin a)))))
    (tag (lambda (x) (attach-tag 'rectangular x))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (install-polar-package)
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z) (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z) (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x))))
    (tag (lambda (x) (attach-tag 'polar x))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; 2.86
;;;;;;;;;;;;;
;; 汎用演算 ;;
;;;;;;;;;;;;;
(define (equ? x y) (apply-generic 'equ? x y))
(define (project x) (apply-generic 'project x))
(define (raise x) (apply-generic 'raise x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;;;;;;;;;;;;
;; 数の生成 ;;
;;;;;;;;;;;;;
(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;;;;;;;;;;;;;;;;;;
;; apply-generic ;;
;;;;;;;;;;;;;;;;;;;
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (or (eq? op 'project) (eq? op 'raise) (eq? op 'equ?))
              (apply proc (map contents args))
              (drop (apply proc (map contents args))))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond [(> (type-height a1) (type-height a2))
                       (apply-generic op (raise a1) a2)]
                      [(< (type-height a1) (type-height a2))
                       (apply-generic op a1 (raise a2))]
                      [else (error "No method for these types"
                                   (list op type-tags))]))
              (error "No method for these types"
                     (list op type-tags)))))))

;;;;;;;;;;;;;;
;; table操作 ;;
;;;;;;;;;;;;;;
(define table '())

(define (put func type contents)
  (set! table (cons (list func type contents) table)))

(define (get func type)
  (letrec ((get-sub (lambda (func type table)
                      (cond [(null? table) #f]
                            [(and (equal? func (caar table))
                                  (equal? type (cadar table)))
                             (caddar table)]
                            [else (get-sub func type (cdr table))]))))
    (get-sub func type table)))

;;;;;;;;;;;;;;;;
;; その他の関数 ;;
;;;;;;;;;;;;;;;;
;; drop演算
(define (drop x)
  (let ((projected (project x)))
    (if (equ? x (raise projected))
        (if (eq? (type-tag projected) 'integer)
            projected
            (drop projected))
        x)))

;; 型の塔における高さ
(define (type-height x)
  (if (eq? (type-tag x) 'complex)
      0
      (+ 1 (type-height (raise x)))))

;; tagの付与
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

;; tagの取得
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))

;; contentsの取得
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))

;;;;;;;;;;;;;;
;; パッケージ ;;
;;;;;;;;;;;;;;
;; 整数パッケージ
(define (install-integer-package)
  (let ((tag (lambda (x) (attach-tag 'integer x))))
    (put 'add '(integer integer)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(integer integer)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(integer integer)
         (lambda (x y) (tag (* x y))))
    (put 'div '(integer integer)
         (lambda (x y) (tag (quotient x y))))
    (put 'equ? '(integer integer)
         (lambda (x y) (= x y)))
    (put 'make 'integer
         (lambda (x) (tag (floor x))))
    (put 'project '(integer)
         (lambda (x) (tag x)))
    (put 'raise '(integer)
         (lambda (x) (make-rational x 1)))
    'done))

;; 有理数パッケージ
(define (install-rational-package)
  (letrec* ((numer (lambda (x) (car x)))
            (denom (lambda (x) (cdr x)))
            (make-rat (lambda (n d)
                        (let ((g (gcd n d)))
                          (cons (/ n g) (/ d g)))))
            (gcd (lambda (a b)
                   (if (zero? b)
                       a
                       (gcd b (remainder a b)))))
            (add-rat (lambda (x y)
                       (make-rat (+ (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (sub-rat (lambda (x y)
                       (make-rat (- (* (numer x) (denom y))
                                    (* (numer y) (denom x)))
                                 (* (denom x) (denom y)))))
            (mul-rat (lambda (x y)
                       (make-rat (* (numer x) (numer y))
                                 (* (denom x) (denom y)))))
            (div-rat (lambda (x y)
                       (make-rat (* (numer x) (denom y))
                                 (* (denom x) (numer y)))))
            (tag (lambda (x) (attach-tag 'rational x))))
           (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
           (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
           (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
           (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))
           (put 'equ? '(rational rational)
                (lambda (x y) (= (/ (numer x) (denom x))
                                 (/ (numer y) (denom y)))))
           (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
           (put 'raise '(rational)
                (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
           (put 'project '(rational)
                (lambda (x) (make-integer (numer x))))
           'done))

;; 実数パッケージ
(define (install-scheme-number-package)
  (let ((tag (lambda (x) (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (put 'raise '(scheme-number)
         (lambda (x) (make-complex-from-real-imag x 0)))
    (put 'project '(scheme-number)
         (lambda (x) (make-rational (floor x) 1)))
    'done))

;; 複素数パッケージ
(define (install-complex-package)
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (r a)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag
                         (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (mul (magnitude z1) (magnitude z2))
                         (add (angle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang
                         (div (magnitude z1) (magnitude z2))
                         (sub (angle z1) (angle z2)))))
         (tag (lambda (z) (attach-tag 'complex z))))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'equ? '(complex complex)
         (lambda (z1 z2) (and (equ? (real-part z1) (real-part z2))
                              (equ? (imag-part z1) (imag-part z2)))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'project '(complex)
         (lambda (z) (make-scheme-number (contents (drop (real-part z))))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;; 直交座標パッケージ
(define (install-rectangular-package)
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z) (sqrt (+ (square (real-part z))
                                         (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a) (cons (* r (cos a)) (* r (sin a)))))
    (tag (lambda (x) (attach-tag 'rectangular x))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;; 極座標パッケージ
(define (install-polar-package)
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z) (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z) (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x))))
    (tag (lambda (x) (attach-tag 'polar x))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))
