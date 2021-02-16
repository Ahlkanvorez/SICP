(ns sicp.ch2-test
  (:require [sicp.ch2 :as ch2]
            [sicp.ch1 :refer [abs]]
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

(deftest ex8-test
  (let [a (ch2/make-interval 1 3)
        b (ch2/make-interval 2 6)]
    (is (= -5 (ch2/lower-bound (ch2/sub-interval a b))))

    (is (= 1 (ch2/upper-bound (ch2/sub-interval a b))))))

(deftest ex9-test
  (let [a (ch2/make-interval 1 3)
        b (ch2/make-interval 2 4)]
    (is (= (+ (ch2/width-interval a) (ch2/width-interval b))
           (ch2/width-interval
            (ch2/add-interval a b)))))

  (let [a (ch2/make-interval 1 3)
        b (ch2/make-interval 2 4)]
    (is (= (+ (ch2/width-interval a) (ch2/width-interval b))
           (ch2/width-interval
            (ch2/sub-interval a b)))))

  (let [a (ch2/make-interval 1 3)
        b (ch2/make-interval 2 4)]
    (is (not= (+ (ch2/width-interval a) (ch2/width-interval b))
              (ch2/width-interval
               (ch2/mul-interval a b)))))
  (let [a (ch2/make-interval 1 3)
        b (ch2/make-interval 2 4)]
    (is (not= (+ (ch2/width-interval a) (ch2/width-interval b))
              (ch2/width-interval
               (ch2/div-interval a b))))))

(deftest ex10-test
  (is (= "Cannot divide by interval spanning 0"
         (try (ch2/div-interval (ch2/make-interval 3 4)
                                (ch2/make-interval -1 1))
              (catch Exception e (ex-message e))))))

(deftest ex11-test
  (doseq [a (range -4 5)]
    (doseq [b (range (inc a) 5)]
      (doseq [c (range -4 5)]
        (doseq [d (range (inc c) 5)]
          (let [x (ch2/make-interval a b)
                y (ch2/make-interval c d)]
            (is (= [x y (ch2/mul-interval x y)]
                   [x y (ch2/mul-interval-via-cases x y)]))))))))

(deftest ex12-test
  (let [i (ch2/make-center-percent 100 0.05)]
    (is (= 0.05 (ch2/percent i)))

    (is (= 95.0 (ch2/lower-bound i)))

    (is (= 105.0 (ch2/upper-bound i)))

    (is (= 5.0 (ch2/width i)))))

(deftest ex13-test
  (letfn [(close-enough? [a b] (< (abs (- a b)) 0.0000001))]
    (doseq [p (range 0.0001 0.0010 0.0001)]
      (doseq [q (range 0.0001 0.0010 0.0001)]
        (let [x (ch2/make-center-percent 100 p)
              y (ch2/make-center-percent 100 q)]
          (is (close-enough? (+ p q) (ch2/percent (ch2/mul-interval x y)))))))))

(deftest ex14-test
  (letfn [(close-enough? [a b] (< (abs (- a b)) 0.0000001))]
    (doseq [p (range 0.0001 0.0010 0.0001)]
      (doseq [q (range 0.0001 0.0010 0.0001)]
        (let [x (ch2/make-center-percent 100 p)
              y (ch2/make-center-percent 100 q)]
          (is (not= (ch2/par1 x y) (ch2/par2 x y))))))))

(deftest ex15-test
    ;; Interval multiplication has a scaling effect which relates to
    ;; rational number multiplication differently than may be expected.
    ;; For example, while (x^2)/x = x holds for rationals, it does not
    ;; hold for intervals; neither does x/(x^2) = 1/x. Minimizing this
    ;; effect by minimizing the number of multiplication operations
    ;; will produce more narrow intervals, since it minimizes the
    ;; scaling effect, thus producing tighter error bounds.
  (let [one (ch2/make-interval 1 1)
        x (ch2/make-interval 1/2 2)]
    (is (not= x (ch2/div-interval (ch2/mul-interval x x) x)))

    (is (not= x (ch2/div-interval x (ch2/mul-interval x x))))))

(deftest ex16-test
  ;; It follows from the above observation of (x^2)/x not being x that
  ;; interval multiplication and division are not inverse operations.
  ;; Consequently, the algebraic properties assumed when asserting the
  ;; equations underlying par1 and par2 are equal for intervals do not
  ;; hold. Thus, while "equivalent" algebraic expressions may seem to
  ;; produce different results, this is due to false algebraic
  ;; assumptions. With interval multiplication and division defined as
  ;; they are here, it is impossible to make these two equations equal
  ;; in general, since doing so requires the above property holds.
  (let [one (ch2/make-interval 1 1)
        x (ch2/make-interval 1/2 2)]
    (is (not= x (ch2/div-interval (ch2/mul-interval x x) x)))

    (is (not= x (ch2/div-interval x (ch2/mul-interval x x))))))

(deftest ex17-test
  (is (= (ch2/cons 34 nil) (ch2/last-pair (ch2/list 23 72 149 34))))
  (is (= nil (ch2/last-pair (ch2/list))))
  (is (= (ch2/cons 1 nil) (ch2/last-pair (ch2/list 1)))))

(deftest ex18-test
  (is (= (ch2/list 25 16 9 4 1) (ch2/reverse (ch2/list 1 4 9 16 25))))

  (is (= (ch2/list) (ch2/reverse (ch2/list))))

  (is (= (ch2/list 1) (ch2/reverse (ch2/list 1))))

  (is (= (ch2/list 2 1) (ch2/reverse (ch2/list 1 2)))))

(deftest ex19-test
  ;; The order of the coin list does not effect the results, because
  ;; all combinations of coins less than or equal to amount are checked,
  ;; with the amounts combined additively, so the combinations are not
  ;; sensitive to order.
  (is (= 4 (ch2/cc 11 ch2/us-coins)))

  (is (= 4 (ch2/cc 11 (ch2/reverse ch2/us-coins)))))

(deftest ex20-test
  (is (= (ch2/list 1 3 5 7) (ch2/same-parity 1 2 3 4 5 6 7)))

  (is (= (ch2/list 2 4 6) (ch2/same-parity 2 3 4 5 6 7))))

(deftest ex21-test
  (is (= (ch2/list 1 4 9 16 25 36 49)
         (ch2/square-list-recursive (ch2/list 1 2 3 4 5 6 7))))

  (is (= (ch2/list 1 4 9 16 25 36 49)
         (ch2/square-list-higher-order (ch2/list 1 2 3 4 5 6 7)))))

