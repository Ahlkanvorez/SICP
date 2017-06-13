(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))

(define (objective-good-enough? guess x)
        (< (abs (- (square guess) x)) 0.0001))

(define (sqrt good-enough?)
        (define (sqrt-iter guess x)
                (if (good-enough? guess x)
                    guess
                    (sqrt-iter (improve guess x)
                               x)))
        sqrt-iter)

(define (relative-good-enough? guess x)
        (let ((improved (improve guess x))
              (threshold 0.0001))
             (< (abs (- guess improved)) threshold)))

;; Testing
(define (test guess)
        (define (percent-error sqrt-f guess x)
                (- 1 (/ (square (sqrt-f guess x)) x)))
        (define (compare x)
                (list (percent-error (sqrt objective-good-enough?) guess x)
                      (percent-error (sqrt relative-good-enough?) guess x)))
        (map compare (list 1e-10 1e-9 1e-8 1e-7 1e-6 1e-5 1e-4 1e-3 1e-2 1e-1 1e1 1e2 1e3 1e4 1e5 1e6 1e7 1e8 1e9 1e10)))

; As the size of x decreases, the effectiveness of the threshold for the objective test decreases much more quickly than that of
; the relative test, since the actual value is so small that its difference from the guess is almost less than the threshold by
; default. The percent error for the relative test is far better for smaller numbers than that of the objective test:
'((-610351.2291260858 -148.6787255115118)
  (-61034.82287706884 -14.572254561591006)
  (-6103.182261898941 -1.1971680312810702)
  (-610.0182976748368 -0.3505105737431087)
  (-60.702871659864435 -0.06882770796199034)
  (-5.780786658404653 -0.0061765790409000765)
  (-0.3631669067247125 -0.00014281284086203883)
  (-0.0722526644431074 -0.0012171682319481736)
  (-0.0065263157858850285 -1.0579156517920296e-05)
  (-0.00011255662039411085 -0.00011255662039411085)
  (-3.1668918598626306e-09 -3.1668918598626306e-09)
  (-2.7979396577393345e-11 -1.0579156517920296e-05)
  (-3.441691376337985e-14 -3.699243660282292e-07)
  (-5.098148792015422e-09 -5.098148792015422e-09)
  (-2.2462698368030942e-11 -2.2462698368030942e-11)
  (-2.353672812205332e-14 -2.353672812205332e-14)
  (2.220446049250313e-16 -3.995851205473855e-09)
  (0.0 -1.64923630308067e-11)
  (-1.5987211554602254e-14 -1.5987211554602254e-14)
  (0.0 0.0))
