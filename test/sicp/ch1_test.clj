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

(comment ;; ex5-test ; invoking (test 0 (p))
  "Invocations of (ch1/test 0 (ch1/p)) will never"
  " terminate, as long as test is a normal function;"
  " i.e. it uses applicative order evaluation, which"
  " will attempt to evaluate the infinitely recursive"
  " definition of p."
  " If test is a macro, i.e. it uses normal order"
  " evaluation, then it will not evaluate p until it"
  " needs the value, so 0 will be returned.")

(comment ;; ex6-test : sqrt-iter using new-if
  "new-if is a function, not a macro, so its arguments are"
  " evaluated before it is invoked (applicative order)."
  " Consequently, the sqrt-iter function will loop forever,"
  " as each invocation of new-if in sqrt-iter will result"
  " in another sqrt-iter call. This will stack overflow,"
  " since the function is not tail recursive.")

(deftest ex7-test
  (testing "sqrt with objective-good-enough?"
    (let [sqrt (ch1/sqrt ch1/objective-good-enough?)]
      (is (< (- (sqrt 1 16) 4)
             0.0001))

      (is (< (- (sqrt 300000 100000000000) 316227.7660)
             0.0001))

      (comment
        "The following test will fail, because the"
        " objective test effectiveness decreases more"
        " rapidly as the value approaches and becomes less"
        " than the threshold."
        (is (< (- (sqrt 0.1 0.000000001) 0.00001)
               0.0001)))))

  (testing "sqrt with relative-good-enough?"
    (let [sqrt (ch1/sqrt ch1/relative-good-enough?)]
      (is (< (- (sqrt 1 16) 4)
             0.0001))

      (is (< (- (sqrt 300000 100000000000) 316227.7660)
             0.0001))

      (comment
        "The following test will fail for a similar reason"
        " to why the equivalent objective test will fail,"
        " however the relative test results in a much closer"
        " value to the actual solution, since the threshold"
        " has a lessened impact since it is compared with"
        " a relative difference."
        (is (< (- (sqrt 0.1 0.000000001) 0.00001)
               0.0001))))))
