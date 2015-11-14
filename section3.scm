;; 3.1
(define (make-accumulator sum)
  (lambda (val)
    (begin (set! sum (+ sum val))
           sum)))

;; 3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond [(eq? x 'how-many-calls?) count]
            [(eq? x 'reset-count) (begin (set! count 0)
                                         count)]
            [else (begin (set! count (+ count 1))
                         (f x))]))))

;; 3.3
(define (make-account balance password)
  (let* ((withdraw (lambda (amount)
                     (if (>= balance amount)
                         (begin (set! balance (- balance amount))
                                balance)
                         "Insufficient funds")))
         (deposit (lambda (amount)
                    (begin (set! balance (+ balance amount))
                           balance)))
         (dispatch (lambda (m p)
                     (if (eq? p password)
                         (cond [(eq? m 'withdraw) withdraw]
                               [(eq? m 'deposit) deposit]
                               [else (error "Unknown request -- MAKE-ACCOUNT"
                                            m)])
                         (lambda (x) "Incorrect password")))))
    dispatch))

;; 3.4
(define (make-account balance password)
  (let ((count 0))
    (let* ((withdraw (lambda (amount)
                       (if (>= balance amount)
                           (begin (set! balance (- balance amount))
                                  balance)
                           "Insufficient funds")))
           (deposit (lambda (amount)
                      (begin (set! balance (+ balance amount))
                             balance)))
           (call-the-cops (lambda (x) "CALL-THE-COPS"))
           (dispatch (lambda (m p)
                       (if (eq? p password)
                           (cond [(eq? m 'withdraw) withdraw]
                                 [(eq? m 'deposit) deposit]
                                 [else (error "Unknown request -- MAKE-ACCOUNT"
                                              m)])
                           (let ((nextcount (+ count 1)))
                             (if (= nextcount 7)
                                 (begin (set! count 0)
                                        call-the-cops)
                                 (begin (set! count nextcount)
                                        (lambda (x) "Incorrect password"))))))))
      dispatch)))

;; 3.5
(use srfi-27)

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (- x1 x2) (- y1 y2) (monte-carlo trials P)))

(define (circle-test x1 x2 y1 y2)
  (lambda ()
    (<= (+ (square (random-in-range x1 x2))
           (square (random-in-range y1 y2)))
        1)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random-real)))))

(define (monte-carlo trials experiment)
  (letrec ((iter (lambda (trials-remaining trials-passed)
                   (cond [(zero? trials-remaining) (/ trials-passed trials)]
                         [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
                         [else (iter (- trials-remaining 1) trials-passed)]))))
    (iter trials 0)))

(define (estimate-pi trials)
  (estimate-integral (circle-test -1.0 1.0 -1.0 1.0) -1.0 1.0 -1.0 1.0 trials))

;; 3.6
(use srfi-27)

(define (rand x)
  (cond [(eq? x 'generate) (random-real)]
        [(eq? x 'reset) (lambda (i j)
                          (random-source-pseudo-randomize!
                           default-random-source i j))]
        [else (error "Incorrect argument -- RAND" x)]))

;; 3.7
(define (make-account balance password-list)
  (letrec* ((withdraw (lambda (amount)
                        (if (>= balance amount)
                            (begin (set! balance (- balance amount))
                                   balance)
                            "Insufficient funds")))
            (deposit (lambda (amount)
                       (begin (set! balance (+ balance amount))
                              balance)))
            (member? (lambda (x lis)
                       (cond [(null? lis) #f]
                             [(eq? x (car lis)) #t]
                             [else (member? x (cdr lis))])))
            (add-access (lambda (password)
                          (begin (set! password-list (cons password password-list))
                                 password)))
            (dispatch (lambda (m p)
                        (if (member? p password-list)
                            (cond [(eq? m 'withdraw) withdraw]
                                  [(eq? m 'deposit) deposit]
                                  [(eq? m 'add-access) add-access]
                                  [else (error "Unknown request -- MAKE-ACCOUNT"
                                               m)])
                            (lambda (x) "Incorrect password")))))
           dispatch))

(define (make-joint account orgpassword newpassword)
  (begin ((account 'add-access orgpassword) newpassword)
         account))
;; 3.8
(define f
  (let ((val 0)
        (count 0))
    (lambda (x)
      (if (zero? count)
          (begin (set! val x)
                 (set! count (+ count 1))
                 val)
          val))))
