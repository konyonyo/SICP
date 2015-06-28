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
; ���ߌn����p�I�����Ȃ�΁A�ȉ���]������Ɩ������[�v�ɓ���͂�
(test 0 (p))
; gauche�Ŏ������疳�����[�v�ɓ�����

; Newton�@�ŕ����������߂�
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
; ��̕����������߂�葱���ɂ����āAif�����`���łȂ����ʂ̎葱��new-if�ɕς�����ǂ��Ȃ邩?
(define new-if
  (lambda (predicate then-clause else-clause)
    (cond
     [predicate then-clause]
     [else else-clause])))
; new-if�̈����ł���predicate, then-clause, else-clause���S�ĕ]������邽�߁A
; �������[�v�ɓ���Ɨ\�z�ł���
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
; gauche�Ŏ��s�����Ƃ���A��͂薳�����[�v�ɓ�����

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
; => ���`�ċA�I�v���Z�X

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
; => ���`�����I�v���Z�X

; 1.10
; Ackermann�֐�
(define A
  (lambda (x y)
    (cond
     [(= y 0) 0]
     [(= x 0) (* 2 y)]
     [(= y 1) 2]
     [else (A (- x 1) (A x (- y 1)))])))

(A 1 10)
(A 0 (A 1 9))
(* 2 (A 1 9)) ; 2��1��
(* 2 (A 0 (A 1 8)))
(* 2 (* 2 (A 1 8))) ; 2��2��
(* 2 (* 2 (A 0 (A 1 7))))
(* 2 (* 2 (* 2 (A 1 7)))) ; 2��3��
(* 2 (* 2 (* 2 (A 0 (A 1 6)))))
(* 2 (* 2 (* 2 (* 2 (A 1 6))))) ; 2��4��
; ...
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 1)))))))))) ; 2��9��
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
; (- n 2) = 0 �Ȃ�� (expt 2 (expt 2 0)) = (expt 2 1)
; (- n 2) = 1 �Ȃ�� (expt 2 (expt 2 2))
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






