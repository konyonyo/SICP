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
