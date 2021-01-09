(ns sicp.ch1-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.ch1 :as ch1]))

(deftest ex1-test
  (is (= 10 10))

  (is (= 12 (+ 5 3 4)))

  (is (= 8 (- 9 1)))

  (is (= 3 (/ 6 2)))

  (is (= 6 (+ (* 2 4) (- 4 6))))

  (let [a 3
        b (+ a 1)]
    (is (= 3 a))

    (is (= 4 b))

    (is (= 19 (+ a b (* a b))))

    (is (= 4
           (if (and (> b a) (< b (* a b)))
             b
             a)))

    (is (= 16
           (cond (= a 4) 6
                 (= b 4) (+ 6 7 a)
                 :else 25)))

    (is (= 6 (+ 2 (if (> b a) b a))))

    (is (= 16
           (* (cond (> a b) a
                    (< a b) b
                    :else -1)
              (+ a 1))))))

(deftest ex2-test
  (is (= -37/150
         (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
            (* 3 (- 6 2) (- 2 7))))))

(deftest ex3-test
  (testing "sum-larger-squares"
    (is (= 5 (ch1/sum-larger-squares 1 2 0)))

    (is (= 5 (ch1/sum-larger-squares 1 0 2)))

    (is (= 5 (ch1/sum-larger-squares 0 2 1)))))

(deftest ex4-test
  (testing "a+abs-b"
    (is (= 11 (ch1/a+abs-b 5 6)))

    (is (= 11 (ch1/a+abs-b 5 -6)))))

