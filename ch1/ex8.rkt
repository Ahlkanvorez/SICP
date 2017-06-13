(define (cube x) (* x (square x)))
(define (square x) (* x x))
(define (improve guess x)
        (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
        (< (abs (- (cube guess) x)) 0.001))

(define (cbrt-iter guess x)
        (if (good-enough? guess x)
            guess
            (cbrt-iter (improve guess x) x)))
