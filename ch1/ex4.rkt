(define (a-plus-abs-b a b)
        ((if (> b 0) + -) a b))
; The operator is chosen based on the sign of b in order to compose the abs function with the normal addition function, i.e.
; the conditional expression evaluated in the operator position acts as the composition of + and abs on the second argument,
; so mathematically it is a function f : QxQ -> Q (a b) |-> a + abs(b), that is written quite concisely.
