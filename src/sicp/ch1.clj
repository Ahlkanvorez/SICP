(ns sicp.ch1)

(defn square [x] (* x x))

(defn sum-of-squares [a b] (+ (square a) (square b)))

(defn sum-larger-squares [a b c]
  (let [a-b (max a b)
        a-c (max a c)
        [m n] (if (= a-b a-c)
                [a (max b c)]
                [a-b a-c])]
    (sum-of-squares m n)))

(defn a+abs-b
  "Perform a + |b|

  This utilizes the fact that ifs are expressions with value
  to determine the appropriate operator (+ or -) based on the
  sign of b, without recourse to an absolute value function."
  [a b]
  ((if (> b 0) + -) a b))

(comment
  (defn p [] (p))

  (defn test [x y]
    (if (= x 0)
      0
      y)))

(defn new-if [condition then-clause else-clause]
  (cond condition then-clause :else else-clause))

(defn abs [x] (if (< x 0) (- x) x))

(defn average [a b] (/ (+ a b) 2))

(comment
  (defn sqrt-iter [guess x]
    (letfn [(good-enough? [guess x]
              (< (abs (- (square guess) x))
                 0.001))
            (improve [guess x]
              (average guess (/ x guess)))]
      (new-if (good-enough? guess x)
              guess
              (sqrt-iter (improve guess x)
                         x)))))

(defn improve [guess x] (average guess (/ x guess)))

(defn objective-good-enough? [guess x]
  (< (abs (- (square guess) x))
     0.0001))

(defn relative-good-enough? [guess x]
  (let [improved (improve guess x)
        threshold 0.0001]
    (< (abs (- guess improved)) threshold)))

(defn sqrt [good-enough?]
  (fn [guess x]
    (if (good-enough? guess x)
      guess
      (recur (improve guess x) x))))

(defn cube [x] (* x (square x)))

(defn cbrt-improve [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(defn cbrt-good-enough? [guess x]
  (< (abs (- (cube guess) x)) 0.001))

(defn cbrt [guess x]
  (if (cbrt-good-enough? guess x)
    guess
    (recur (cbrt-improve guess x) x)))

(defn +-recursive [a b]
  (if (= a 0)
    b
    (inc (+-recursive (dec a) b))))

(defn +-iterative [a b]
  (if (= a 0)
    b
    (recur (dec a) (inc b))))

(defn A
  "Ackermann's function

  Note that the definition here (as given in the book)
  differs from the one on Wikipedia."
  [x y]
  (cond (zero? y) 0
        (zero? x) (* 2 y)
        (= 1 y) 2
        :else (A (dec x) (A x (dec y)))))

(defn f
  "(A 0 n) = 2n"
  [n] (A 0 n))

(defn g
  "(A 1 n) = 2^n"
  [n]
  (A 1 n))

(defn h
  "(A 2 n) = 2^^n
   (A 2 n) = 2^^(n + 3) - 3
  where ^^ is the knuth up-arrow, tetration"
  [n]
  (A 2 n))

(defn recursive-f [n]
  (cond (< n 3) n
        :else (+ (recursive-f (dec n))
                 (* 2 (recursive-f (- n 2)))
                 (* 3 (recursive-f (- n 3))))))

(defn iterative-f [n]
  (loop [m 3
         memo {0 0
               1 1
               2 2}]
    (if (> m n)
      (get memo n)
      (recur (inc m)
             (assoc memo m
                    (+ (get memo (dec m))
                       (* 2 (get memo (- m 2)))
                       (* 3 (get memo (- m 3)))))))))

(defn pascal [r c]
  (if (or (zero? r) (zero? c) (= r c))
    1
    (+ (pascal (dec r) c)
       (pascal (dec r) (dec c)))))

(defn first-denomination [kinds-of-coins]
  (case kinds-of-coins
    1 1
    2 5
    3 10
    4 25
    5 50))

(defn cc [amount kinds-of-coins]
  (cond (zero? amount) 1
        (or (< amount 0) (zero? kinds-of-coins)) 0
        :else (+ (cc amount (dec kinds-of-coins))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins))))

(defn cube [x] (* x x x))

(defn p [x] (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (dec n)))))

(defn fast-expt [b n]
  (cond (zero? n) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (dec n)))))

(defn fast-expt-iter
  "Ex. 1.16: Compute b^n in O(lg n) time, O(1) space.
  This implements the following in an iterative recursive process:
    b^n = (b^(n/2))^2     when 2 | n
    b^n = b * b^(n - 1)   otherwise.

  In the loop, a is defined such that a(b^n) remains constant in each
  iteration. The changes in each recur are thus derived:
  b -> b - 1; a(b^n) -> (ab)b^(n - 1)
    thus (* a b), b, (dec n)
  b -> b / 2; a(b^n) -> a(b^(n/2))b^(n/2) = a(b^(n/2))^2 = a(b^2)^(n/2)
    thus a, (square b), (/ n 2)."
  [b n]
  (loop [n n
         b b
         a 1]
    (if (= n 0)
      a
      (if (odd? n)
        (recur (dec n) b (* a b))
        (recur (/ n 2) (square b) a)))))

(defn fast-* [a b]
  (cond (zero? b) 0
        (even? b) (* 2 (fast-* a (/ b 2)))
        :else (+ a (fast-* a (dec b)))))

(defn fast-*-iter
  "Ex 1.18: Compute a*b in O(lg n) time, O(1) space.
  This is based on the same observation as fast-expt-iter regarding
  binary methods (see Knuth 4.6.3, specifically vol 2 ed 3 pg 462).
  First observe:
    ab = (2a)(b/2) = (a + a)(b/2)   when 2 | a
    ab = a(b - 1) + a               otherwise

  Then, in the loop, define x such that x + ab remains constant each
  iteration:
    b -> b - 1; x + ab = x + (a(b - 1) + a) = (b - 1)a + (x + a)
      thus (dec b) a (+ x a)

    b -> b / 2; x + ab = x + (2a)(b / 2) = (b / 2)(a + a) + x
      thus (/ b 2) (+ a a) x.

  These two procedures illustrate a general binary method for optimizing
  recursive logarithmic procedures into iterative ones, via an invariant
  combination of the loop variables. Note the mappings of functions
  between the expt example and this one:
      ^   ->   *
      *   ->   +
   square -> double
    sqrt  -> halve
"
  [a b]
  (loop [b b
         a a
         x 0]
    (if (= b 0)
      x
      (if (odd? b)
        (recur (dec b) a (+ x a))
        (recur (/ b 2) (+ a a) x)))))
