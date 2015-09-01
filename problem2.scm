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



