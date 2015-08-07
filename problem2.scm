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
;; 2‚Æ3‚Í‘f”‚Å‚ ‚é‚©‚çA‘fˆö”•ª‰ğ‚ÌˆêˆÓ«‚æ‚è•\Œ»‰Â”\‚Å‚ ‚éB

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
;; ˜a
(define (add-interval-width x-width y-width)
  (/ (+ x-width y-width) 2))

;; ·
(define (sub-interval-width x-width y-width)
  (/ (+ x-width y-width) 2))

;; æ
;; ”½—á -> p1‚ªmin, p2‚ªmax‚Ì‚Æ‚«
;;        [x1, x2] * [y1, y2] = [x1*y1, x1*y2]
;;        • = x1*y2 - x1*y1 = x1*(y2 - y1)
;;        ‚±‚ê‚Í•‚¾‚¯‚ÌŠÖ”‚Å‚Í‚È‚¢B

;; œ
;; œZ‚ÍæZ‚ğg—p‚µ‚Ä‚¢‚é‚½‚ßAœZŒ‹‰Ê‚Ì•‚à•‚¾‚¯‚ÌŠÖ”‚Å‚Í‚È‚¢B

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
;; “ñ‚Â‚Ì‹æŠÔ‚ÌÏ‚Ì‘Š‘Î‹–—eŒë· p = (x*dy+y*dx)/x*y

;; 2.14
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

(define (div-interval x y)
  (if (and (negative? (lower-bound y)) (positive? (upper-bound y)))
      (error "lower-bound is negative and upper-bound is positive")
      (mul-interval x (make-interval (/ 1 (upper-bound y))
				     (/ 1 (lower-bound y))))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
