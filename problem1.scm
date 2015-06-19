; 1.2
(/ (+ 5
      4
      (- 2 (- 3
	      (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; 1.3
(define large-square
  (lambda (a b c)
    (cond
     [(and (<= a b) (<= a c)) (+ (* b b) (* c c))]
     [(and (<= b a) (<= b c)) (+ (* a a) (* c c))]
     [else (+ (* a a) (* b b))])))

; 1.4
(define a-plus-abs-b
  (lambda (a b)
    ((if (> b 0) + -) a b)))

; 1.5
(define p
  (lambda ()
    (p)))

(define test
  (lambda (x y)
    (if (= x 0)
	0
	y)))
; 解釈系が作用的順序ならば、以下を評価すると無限ループに入るはず
(test 0 (p))
; gaucheで試したら無限ループに入った

; Newton法で平方根を求める
(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x) x))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

; 1.6
; 上の平方根を求める手続きにおいて、ifを特殊形式でない普通の手続きnew-ifに変えたらどうなるか?
(define new-if
  (lambda (predicate then-clause else-clause)
    (cond
     [predicate then-clause]
     [else else-clause])))
; new-ifの引数であるpredicate, then-clause, else-clauseが全て評価されるため、
; 無限ループに入ると予想できる
(define sqrt-iter
  (lambda (guess x)
    (new-if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x) x))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))
; gaucheで実行したところ、やはり無限ループに入った

; 1.7
(define sqrt-iter
  (lambda (guess x guess-prev)
    (if (good-enough? guess guess-prev)
	guess
	(sqrt-iter (improve guess x) x guess))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess guess-prev)
    (< (abs (/ (- guess guess-prev) guess)) 0.001)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x x)))

; 1.7
(define cubic-iter
  (lambda (guess x guess-prev)
    (if (good-enough? guess guess-prev)
	guess
	(sqrt-iter (improve guess x) x guess))))

(define improve
  (lambda (guess x)
    (average guess (/ (+ (/ x (* guess guess))
			 (* 2 guess))
		      3))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess guess-prev)
    (< (abs (/ (- guess guess-prev) guess)) 0.001)))

(define cubic
  (lambda (x)
    (cubic-iter 1.0 x x)))
