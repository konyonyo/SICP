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
