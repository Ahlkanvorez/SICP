(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (objective-good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))

(define good-enough? objective-good-enough?)

(define (sqrt-iter guess x)
        (define (improve guess x) (average guess (/ x guess)))
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x)
                       x)))
