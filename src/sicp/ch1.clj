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


