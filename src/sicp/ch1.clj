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

(defn fib-iter [n]
  (loop [a 1
         b 0
         n n]
    (if (zero? n)
      b
      (recur (+ a b) a (dec n)))))

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

(defn fib
  "Compute the nth fibonacci number in O(log n) time and O(1) space.

  This solution builds on the classic iterative O(n) algorithm for
  calculating fibonacci numbers using the same insight that produces
  logarithmic exponentiation and multiplication algorithms from their
  linear iterative variants. For context, recall that the nth fibonacci
  number can be computed in O(n) time and O(1) space as follows:
  Observe the transformation T(a, b) = [a + b, a], and note that
  T^n[1 0] = [fib(n + 1), fib(n)]. Moreover, note that T is a linear
  map, and thus has a matrix representation:

  M(T) = [ 1 1 ] since [ 1 1 ][ a ] = [ a + b ]
         [ 1 0 ]       [ 1 0 ][ b ] = [   a   ]

  Now, recall that the logarithmicizing process from before involved
  identifying a \"square\" or \"double\" operation, by which we can
  reduce the remaining operations to perform by half. In that light,
  observe the forms of M^2(T) and M^4(T):

  M^2(T) = [ 1 1 ][ 1 1 ] = [ 2 1 ]
           [ 1 0 ][ 1 0 ]   [ 1 1 ]

  M^4(T) = [ 2 1 ][ 2 1 ] = [ 5 3 ]
           [ 1 1 ][ 1 1 ]   [ 3 2 ]

  Now, observe a pattern (not proven in general here) that these
  matrices are of the form [ a + b  b ]
                           [   b    a ].

  Then, to \"double\" or \"square\" a matrix M^2k(T), we can identify
  the square of that general form (assuming * is commutative here):

  [ a + b  b ][ a + b  b ] = [ a^2 + 2ab + 2b^2  2ab + b^2 ]
  [   b    a ][   b    a ]   [    2ab + b^2      a^2 + b^2 ]

  Now, this matrix is also of the pattern noted above, where
    p = a^2 +       b^2
    q =       2ab + b^2

  since
    p + q = a^2 + 2ab + 2b^2.

  Thus, when we are at a stage of the algorithm with an even n, we
  can \"square the transformation\" by replacing p & q with those new
  values to halve n and reach the result in a logarithmic number of
  steps.

  The next piece to the solution is identifying the transform for the
  case where n is odd, in which we want to apply the above transform
  once instead of squaring the transform. We can identify the formulas
  by multiplying the matrix of the pattern above with [ a b ]:

  [ p + q   q ][ a ] = [ pa + qa + qb ]
  [   q     p ][ b ]   [    qa + pb   ]

  Lastly, observe that the initial values of p & q are given by the
  matrix form of T:

  [ p + q  q ] = [ 1 1 ]
  [   q    p ] = [ 1 0 ],

  so p = 0, q = 1.
  "
  [n]
  (loop [a 1
         b 0
         p 0
         q 1
         n n]
    (cond (zero? n) b
          (even? n) (recur a
                           b
                           (+ (square p) (square q))
                           (+ (* 2 p q) (square q))
                           (quot n 2))
          :else (recur (+ (* p a) (* q a) (* q b))
                       (+ (* q a) (* p b))
                       p
                       q
                       (dec n)))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn divides? [k n]
  (zero? (mod n k)))

(defn smallest-divisor [n]
  (loop [divisor 2]
    (cond (< n (square divisor)) n
          (divides? divisor n) divisor
          :else (recur (inc divisor)))))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn exp-mod [base exp m]
  (cond (zero? exp) 1
        (even? exp) (mod (square (exp-mod base (quot exp 2) m)) m)
        :else (mod (* base (exp-mod base (dec exp) m)) m)))

(defn fermat-test [n]
  (let [a (inc (rand-int (dec n)))]
    (= a (exp-mod a n n))))

(defn fast-prime? [n times]
  (cond (zero? times) true
        (fermat-test n) (recur n (dec times))
        :else false))

(defmacro runtime [expr]
  `(let [start# (System/nanoTime)
         ret# ~expr
         duration# (/ (- (System/nanoTime) start#) 1000000.0)]
     [ret# duration#]))

(defn profile [thunk times]
  (transduce (comp (map (fn [_] (second (runtime (thunk)))))
                   (remove zero?))
             min ##Inf
             (range times)))

(defn runtimes-between [f low high]
  (loop [n (filter odd? (range low high))
         accum []]
    (if-let [k (first n)]
      (let [[p t] (runtime (f k))]
        (if p
          (recur (rest n) (conj accum (profile (fn [] (f k)) 100)))
          (recur (rest n) accum)))
      accum)))

(defn prime-runtimes-between [low high]
  (runtimes-between prime? low high))

(defn smallest-divisor-2 [n]
  (if (divides? 2 n)
    2
    (loop [divisor 3]
      (cond (< n (square divisor)) n
            (divides? divisor n) divisor
            :else (recur (unchecked-add-int 2 divisor))))))

(defn prime-2? [n]
  (= n (smallest-divisor-2 n)))

(defn fast-prime-runtimes-between [low high]
  (runtimes-between #(fast-prime? % 5) low high))

(defn bad-expmod [b e m]
  (mod (fast-expt b e) m))
