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
