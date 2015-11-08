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
