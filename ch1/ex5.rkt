(define (p) (p))

(define (test x y)
        (if (= x 0)
            0
            y))
; If the interpreter uses applicative order evaluation, i.e. it evaluates arguments before functional expressions, then his
; test (test 0 (p)) will never terminate, because the argument (p) is recursively defiend and cannot be evaluated in finite time.
; However, if the interpreter uses normal order evaluation, i.e. it evaluates arguments only when their evaluation is required,
; then (test 0 (p)) will return 0, because the second argument is only evaluated when the first is non-zero, but it is zero here.
