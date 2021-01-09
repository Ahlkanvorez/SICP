(ns sicp.ch1)

(defn sum-larger-squares [a b c]
  (letfn [(square [x] (* x x))
          (sum-of-squares [a b] (+ (square a) (square b)))]
    (let [a-b (max a b)
          a-c (max a c)
          [m n] (if (= a-b a-c)
                  [a (max b c)]
                  [a-b a-c])]
      (sum-of-squares m n))))

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

