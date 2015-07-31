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

; 1.8
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

; 1.9
(define +
  (lambda (a b)
    (if (= a 0)
	b
	(inc (+ (dec a) b)))))

(+ 5 6)
(inc (+ (dec 5) 6))
(inc (inc (+ (dec 4) 6)))
(inc (inc (inc (+ (dec 3) 6))))
(inc (inc (inc (inc (+ (dec 2) 6)))))
(inc (inc (inc (inc (inc (+ (dec 1) 6))))))
(inc (inc (inc (inc (inc 6)))))
(inc (inc (inc (inc 7))))
(inc (inc (inc 8)))
(inc (inc 9))
(inc 10)
11
; => 線形再帰的プロセス

(define +
  (lambda (a b)
    (if (= a 0)
	b
	(+ (dec a) (inc b)))))

(+ 5 6)
(+ 4 7)
(+ 3 8)
(+ 2 9)
(+ 1 10)
11
; => 線形反復的プロセス

; 1.10
; Ackermann関数
(define A
  (lambda (x y)
    (cond
     [(= y 0) 0]
     [(= x 0) (* 2 y)]
     [(= y 1) 2]
     [else (A (- x 1) (A x (- y 1)))])))

(A 1 10)
(A 0 (A 1 9))
(* 2 (A 1 9)) ; 2が1個
(* 2 (A 0 (A 1 8)))
(* 2 (* 2 (A 1 8))) ; 2が2個
(* 2 (* 2 (A 0 (A 1 7))))
(* 2 (* 2 (* 2 (A 1 7)))) ; 2が3個
(* 2 (* 2 (* 2 (A 0 (A 1 6)))))
(* 2 (* 2 (* 2 (* 2 (A 1 6))))) ; 2が4個
; ...
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 1)))))))))) ; 2が9個
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
; => 2^10

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 (- 3 1))))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 (* 2 2)))
(A 1 (A 1 4))
(A 1 16)
; => 2^16

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 4)
; => 2^16

(define f
  (lambda (n)
    (* 2 n)))

(define g
  (lambda (n)
    (expt 2 n)))

(A 2 n)
(A 1 (A 2 (- n 1)))
(expt 2 (A 2 (- n 1)))
(expt 2 (expt 2 (A 2 (- n 2))))
; (- n 2) = 0 ならば (expt 2 (expt 2 0)) = (expt 2 1)
; (- n 2) = 1 ならば (expt 2 (expt 2 2))
(expt 2 (expt 2 (A 1 (A 2 (- n 3)))))
(expt 2 (expt 2 (expt 2 (A 2 (- n 3)))))
(expt 2 (expt 2 (expt 2 (expt 2 (A 2 (- n 4))))))

(define h
  (lambda (n)
    (if (= n 1)
	2
	(expt 2 (h (- n 1))))))

; 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
  (if (< n 3)
      n
      (f-iter 4 2 1 0 n)))

(define (f-iter m0 m1 m2 m3 count)
  (if (= count 3)
      m0
      (f-iter (+ m0 (* 2 m1) (* 3 m2))
	      m0
	      m1
	      m2
	      (- count 1))))

; 1.12
(define (p n r)
  (if (or (= n 0) (= r 0) (= r n))
      1
      (+ (p (- n 1) (- r 1))
	 (p (- n 1) r))))

; 1.16
(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond
   [(= n 0) a]
   [(even? n) (expt-iter (* b b) (/ n 2) a)]
   [else (expt-iter b (- n 1) (* a b))]))

; 1.17
(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (fast-* a b)
  (cond
   [(= b 0) 0]
   [(even? b) (double (fast-* a (halve b)))]
   [else (+ a (fast-* a (- b 1)))]))

; 1.18
(define (fast-* a b)
  (*-iter a b 0))

(define (*-iter a b n)
  (cond
   [(= b 0) n]
   [(even? b) (*-iter (double a) (halve b) n)]
   [else (*-iter a (- b 1) (+ n a))]))

; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q n)
  (cond
   [(= n 0) b]
   [(even? n) (fib-iter a
			b
			(+ (* p p) (* q q))
			(+ (* 2 p q) (* q q))
			(/ n 2))]
   [else (fib-iter (+ (* b q) (* a q) (* a p))
		   (+ (* b p) (* a q))
		   p
		   q
		   (- n 1))]))

; 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
	[(divides? test-divisor n) test-divisor]
	[else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

; (smallest-divisor 199) => 199
; (smallest-divisor 1999) => 1999
; (smallest-divisor 19999) => 7

; 1.22
(use srfi-19)

(define (search-for-primes idx max)
  (cond [(> idx max) (newline)
	             (display "end")]
	[else (cond [(even? idx) (search-for-primes (+ idx 1) max)]
		    [else (timed-prime-test idx)
			  (search-for-primes (+ idx 2) max)])]))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (time-difference (current-time) start-time))))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
	[(divides? test-divisor n) test-divisor]
	[else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

; 1.23
(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
	[(divides? test-divisor n) test-divisor]
	[else (find-divisor n (next test-divisor))]))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

; 1.24
; (prime? n) -> (fast-prime? n 10)

; 1.25
; 使える。

; 1.26
; *の引数が2回評価されるから。

; 1.27
(define (prime? p)
  (if (= p 1)
      #f
      (prime-iter p p)))

(define (prime-iter p n)
  (cond [(= n 1) #t]
	[else (and (= (expmod (- n 1) p p) (- n 1))
		   (prime-iter p (- n 1)))]))

(define (expmod base exp m)
  (cond [(= exp 0) 1]
	[(even? exp) (remainder (square (expmod base (/ exp 2) m)) m)]
	[else (remainder (* base (expmod base (- exp 1) m)) m)]))

; 1.28
(use srfi-27)

(define (expmod base exp m)
  (cond [(= exp 0) 1]
	[(even? exp) (miller-rabin (expmod base (/ exp 2) m) m)]
	[else (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (miller-rabin a m)
  (if (and (not (= a 1))
	   (not (= a (- m 1)))
	   (= (remainder (square a) m) 1))
      0
      (remainder (square a) m)))

(define (miller-rabin-test n)
  (let ((try-it (lambda (a)
                    (= (expmod a (- n 1) n) 1))))
    (try-it (random-integer n))))

(define (miller-rabin-prime? n times)
  (cond [(= times 0) #t]
	[(miller-rabin-test n) (miller-rabin-prime? n (- times 1))]
	[else #f]))

; 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b n)
  (let* ((h (/ (- b a) n))
	(inc (lambda (x) (+ x 1)))
	(term (lambda (k)
		(cond [(or (= k 0) (= k n)) (f (+ a (* k h)))]
		      [(odd? k) (* 4 (f (+ a (* k h))))]
		      [else (* 2 (f (+ a (* k h))))]))))
    (* (sum term 0 inc n) (/ h 3.0))))

; 1.30
(define (sum term a next b)
  (letrec ((iter (lambda (a result)
		   (if (> a b)
		       result
		       (iter (next a) (+ (term a) result))))))
    (iter a 0)))

; 1.31 a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (let ((term (lambda (x) x))
	(inc (lambda (x) (+ x 1))))
    (if (= n 0)
	1
	(product term 1 inc n))))

(define (pi n)
  (let ((term (lambda (x) (* (/ (* 2 x) (- (* 2 x) 1))
			     (/ (* 2 x) (+ (* 2 x) 1)))))
	(inc (lambda (x) (+ x 1))))
    (* 2.0 (product term 1 inc n))))

; 1.31 b
(define (product term a next b)
  (letrec ((iter (lambda (a result)
		   (if (> a b)
		       result
		       (iter (next a) (* (term a) result))))))
    (iter a 1)))

; 1.32 a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate (lambda (x y) (+ x y))
	      0 term a next b))

(define (product term a next b)
  (accumulate (lambda (x y) (* x y))
	      1 term a next b))

; 1.32 b
(define (accumulate combiner null-value term a next b)
  (letrec ((iter (lambda (a result)
		   (if (> a b)
		       result
		       (iter (next a) (combiner (term a) result))))))
    (iter a null-value)))

; 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (letrec ((iter (lambda (a result)
		   (cond [(> a b) result]
			 [(filter a) (iter (next a)
					   (combiner (term a) result))]
			 [else (iter (next a) result)]))))
    (iter a null-value)))

; 1.33 a
(define (sum-square-prime a b)
  (filtered-accumulate (lambda (x y) (+ x y))
		       0
		       square
		       a
		       (lambda (x) (+ x 1))
		       b
		       prime?))

(define (filtered-accumulate combiner null-value term a next b filter)
  (letrec ((iter (lambda (a result)
		   (cond [(> a b) result]
			 [(filter a) (iter (next a)
					   (combiner (term a) result))]
			 [else (iter (next a) result)]))))
    (iter a null-value)))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
	[(divides? test-divisor n) test-divisor]
	[else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

; 1.33 b
(define (product-coprime n)
  (filtered-accumulate (lambda (x y) (* x y))
		       1
		       (lambda (x) x)
		       1
		       (lambda (x) (+ x 1))
		       (- n 1)
		       (lambda (x) (= 1 (gcd x n)))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (letrec ((iter (lambda (a result)
		   (cond [(> a b) result]
			 [(filter a) (iter (next a)
					   (combiner (term a) result))]
			 [else (iter (next a) result)]))))
    (iter a null-value)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 1.34
; (f f) => (f 2) => (2 2) => error

; 1.35
; φ^2-φ-1=0
; 両辺φで割って
; φ-1-1/φ=0
; 1+1/φ=φ
; φはf(x) (f(x)=1+1/x)の不動点
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (letrec* ((close-enough? (lambda (v1 v2)
			     (< (abs (- v1 v2)) tolerance)))
	    (try (lambda (guess)
		   (let ((next (f guess)))
		     (if (close-enough? guess next)
			 next
			 (try next))))))
	   (try first-guess)))

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; 1.36
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (letrec* ((close-enough? (lambda (v1 v2)
			     (< (abs (- v1 v2)) tolerance)))
	    (try (lambda (guess)
		   (let ((next (f guess)))
		     (begin
		       (display next)
		       (newline)
		       (if (close-enough? guess next)
		           next
			   (try next)))))))
	   (try first-guess)))

(define (average a b)
  (/ (+ a b) 2))

; 平均緩和法を使わないとき
(define answer1 (fixed-point (lambda (x) (/ (log 1000) (log x))) 5.0))

; 平均緩和法を使ったとき
(define answer2 (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
			     5.0))

; 1.37a
(define (cont-frac n d k)
  (letrec ((sub (lambda (i)
		  (if (= i k)
		      (/ (n k) (d k))
		      (/ (n i) (+ (d i) (sub (+ i 1))))))))
    (sub 1)))

(define (phi-k k)
    (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))

(define phi-calc
  (letrec* ((close-enough? (lambda (v1 v2)
			     (< (abs (- v1 v2)) 0.00001)))
	    (sub (lambda (k)
		   (if (close-enough? (phi-k k) (phi-k (+ k 1)))
		       (+ k 1)
		       (sub (+ k 1))))))
	   (sub 1)))

; 1.37b
(define (cont-frac n d k)
  (letrec ((iter (lambda (i result)
		   (if (= i 1)
		       result
		       (iter (- i 1) (/ (n (- i 1))
					(+ (d (- i 1)) result)))))))
    (iter k (/ (n k) (d k)))))

; 1.38
(define (cont-frac n d k)
  (letrec ((iter (lambda (i result)
		   (if (= i 1)
		       result
		       (iter (- i 1) (/ (n (- i 1))
					(+ (d (- i 1)) result)))))))
    (iter k (/ (n k) (d k)))))

(define napier
  (+ 2 (cont-frac (lambda (i) 1.0)
		  (lambda (i)
		    (if (= (remainder i 3) 2)
			(expt 2.0 (+ 1 (/ (- i 2) 3)))
			1.0))
		  100)))

; 1.39
(define (cont-frac n d k)
  (letrec ((iter (lambda (i result)
		   (if (= i 1)
		       result
		       (iter (- i 1) (/ (n (- i 1))
					(+ (d (- i 1)) result)))))))
    (iter k (/ (n k) (d k)))))

(define (tan-cf x k)
  (cont-frac (lambda (i)
	       (if (= i 1)
		   x
		   (* -1 (expt x i))))
	     (lambda (i) (- (* 2 i) 1))
	     k))

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

; 1.41
; => 21

; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

(define (smooth-n f n)
  ((repeated smooth n) f))

; 1.45
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (letrec* ((close-enough? (lambda (v1 v2)
			     (< (abs (- v1 v2)) tolerance)))
	    (try (lambda (guess)
		   (let ((next (f guess)))
		     (if (close-enough? guess next)
			 next
			 (try next))))))
	   (try first-guess)))

(define (n-root a n repeat)
  (fixed-point ((repeated average-damp repeat)
		(lambda (x) (/ a (expt x (- n 1)))))
	       1.0))

; 1.46
(define (iterative-improve enough? improve)
  (lambda (guess)
    (let ((next (improve guess)))
      (if (enough? guess next)
	  next
	  ((iterative-improve enough? improve) next)))))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  ((iterative-improve (lambda (v1 v2)
			(< (abs (- (square v2) x)) 0.001))
		      (lambda (guess)
			(average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (v1 v2)
			(< (abs (- v1 v2)) tolerance))
		      f)
   first-guess))
