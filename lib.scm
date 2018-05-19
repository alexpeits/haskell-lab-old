(define (not x)            (if x #f #t))
(define (null? obj)        (if (eqv? obj '()) #t #f))

(define (id obj)           obj)

(define (curry func arg1)  (lambda (arg) (func (cons arg1 (list arg)))))
(define (compose f g)      (lambda (arg) (f (g arg))))

(define zero?              (curry = 0))
(define positive?          (curry < 0))
(define negative?          (curry > 0))
(define (odd? num)         (= (mod num 2) 1))
(define (even? num)        (= (mod num 2) 0))
