(ns sicp.ch2-test
  (:require [sicp.ch2 :as ch2]
            [clojure.test :refer [deftest is]]))

(def one-third (ch2/make-rat 1 3))

(deftest ex1-test
  (is (= "1/3" (ch2/format-rat one-third)))
  (is (= "-1/3" (ch2/format-rat (ch2/make-rat -1 3))))
  (is (= "-1/3" (ch2/format-rat (ch2/make-rat 1 -3))))
  (is (= "1/3" (ch2/format-rat (ch2/make-rat -1 -3))))
  (is (= "2/3" (ch2/format-rat (ch2/add-rat one-third one-third)))))

(deftest ex2-test
  (is (= "(1, 1)" (ch2/format-point
                   (ch2/midpoint-segment
                    (ch2/make-segment (ch2/make-point 0 0)
                                      (ch2/make-point 2 2))))))

  (is (= "(6, 6)" (ch2/format-point
                   (ch2/midpoint-segment
                    (ch2/make-segment (ch2/make-point 11 11)
                                      (ch2/make-point 1 1))))))

  (is (= "(0, -5)" (ch2/format-point
                   (ch2/midpoint-segment
                    (ch2/make-segment (ch2/make-point -5 0)
                                      (ch2/make-point 5 -10)))))))

(deftest ex3-test
  (is (= 4 (ch2/perimeter-rectangle
            (ch2/make-rectangle
             (ch2/make-point 0 0)
             (ch2/make-point 0 1)
             (ch2/make-point 1 1)
             (ch2/make-point 1 0)))))

  (is (= 8 (ch2/perimeter-rectangle
            (ch2/make-rectangle
             (ch2/make-point 0 0)
             (ch2/make-point 0 2)
             (ch2/make-point 2 2)
             (ch2/make-point 2 0)))))

  (is (= 1 (ch2/area-rectangle
            (ch2/make-rectangle
             (ch2/make-point 0 0)
             (ch2/make-point 0 1)
             (ch2/make-point 1 1)
             (ch2/make-point 1 0)))))

  (is (= 4 (ch2/area-rectangle
            (ch2/make-rectangle
             (ch2/make-point 0 0)
             (ch2/make-point 0 2)
             (ch2/make-point 2 2)
             (ch2/make-point 2 0)))))

  (is (= 4 (ch2/perimeter-rectangle
            (ch2/segments->rectangle
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 1 0))
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 0 1))))))

  (is (= 8 (ch2/perimeter-rectangle
            (ch2/segments->rectangle
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 2 0))
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 0 2))))))

  (is (= 1 (ch2/area-rectangle
            (ch2/segments->rectangle
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 1 0))
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 0 1))))))

  (is (= 4 (ch2/area-rectangle
            (ch2/segments->rectangle
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 2 0))
             (ch2/make-segment (ch2/make-point 0 0) (ch2/make-point 0 2)))))))

(deftest ex4-test
  (is (= 1 (ch2/car-fn (ch2/cons-fn 1 2))))
  (is (= 2 (ch2/cdr-fn (ch2/cons-fn 1 2)))))

(deftest ex5-test
  (is (= 5 (ch2/car-num (ch2/cons-num 5 6))))
  (is (= 6 (ch2/cdr-num (ch2/cons-num 5 6)))))

(deftest ex6-test
  (is (= (((ch2/add-1 ch2/zero) inc) 0) ((ch2/one inc) 0)))

  (is (= (((ch2/add-1 (ch2/add-1 ch2/zero)) inc) 0) ((ch2/two inc) 0)))

  (is (= 3 (((ch2/add-church-numerals ch2/one ch2/two) inc) 0))))

(deftest ex7-test
  (is (= 3 (ch2/lower-bound (ch2/make-interval 3 5))))

  (is (= 5 (ch2/upper-bound (ch2/make-interval 3 5)))))
