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
