(define (new-if predcate then-clause else-clause)
        (cond (predicate then-clause)
              (else else-clause)))

(define (sqrt-iter guess x)
        (define (good-enough? guess x)
                (define (square x) (* x x))
                (define (abs x) (if (< x 0) (- x) x))
                (< (abs (- (square guess) x)) 0.001))
        (define (improve guess x)
                (define (average a b) (/ (+ a b) 2))
                (average guess (/ x guess)))
        (new-if (good-enough? guess x)
                guess
                (sqrt-iter (improve guess x)
                           x)))

; This new-if is defined at the function level, not the macro level, and will therefore evaluate both branches given as arguments
; prior to checking the condition, and then return the return value associated with the proper branch. However, since it is used
; in the context of an iteratively recursive function, it will never terminate, because each attempt to evaluate the arguments
; to the new-if function will result in another recursive call and another new-if. Thus, this sqrt-iter function will never terminate.
