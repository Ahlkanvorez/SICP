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

(deftest ex8-test
  (testing "cbrt"
    (is (< (- (ch1/cbrt 1 8) 2)
           0.001))

    (is (< (- (ch1/cbrt 1 18) 3)
           0.001))

    (is (< (- (ch1/cbrt 1 64) 4)
           0.001))))

(deftest ex9-test
  (testing "+-recursive"
    (is (= 9 (ch1/+-recursive 4 5)))
    ;; Where + = +_recursive:
    ;; > (+ 4 5)
    ;; = (inc (+ 3 5))
    ;; = (inc (inc (+ 2 5)))
    ;; = (inc (inc (inc (+ 1 5))))
    ;; = (inc (inc (inc (inc (+ 0 5)))))
    ;; = (inc (inc (inc (inc 5))))
    ;; = (inc (inc (inc 6)))
    ;; = (inc (inc 7))
    ;; = (inc 8)
    ;; = 9
    )

  (testing "+-iterative"
    (is (= 9 (ch1/+-iterative 4 5)))
    ;; Where + = +_iterative
    ;; > (+ 4 5)
    ;; = (+ 3 6)
    ;; = (+ 2 7)
    ;; = (+ 1 8)
    ;; = (+ 0 9)
    ;; = 9
    ))

(deftest ex10-test
  (testing "A"
    (is (= 1024 (ch1/A 1 10)))
    ;; > (A 1 10) 
    ;; = (A 0 (A 1 9))
    ;; = (* 2 (A 0 (A 1 8)))
    ;; = (* 2 2 (A 0 (A 1 7)))
    ;; = (* 2 2 2 (A 0 (A 1 6)))
    ;; = (* 2 2 2 2 (A 0 (A 1 5)))
    ;; = (* 2 2 2 2 2 (A 0 (A 1 4)))
    ;; = (* 2 2 2 2 2 2 (A 0 (A 1 3)))
    ;; = (* 2 2 2 2 2 2 2 (A 0 (A 1 2)))
    ;; = (* 2 2 2 2 2 2 2 2 (A 0 (A 1 1)))
    ;; = (* 2 2 2 2 2 2 2 2 2 (A 1 2))
    ;; = (* 2 2 2 2 2 2 2 2 2 2)
    ;; = 1024 = 2^10

    (is (= 65536 (ch1/A 2 4)))
    ;; > (A 2 4)
    ;; = (A 1 (A 2 3))
    ;; = (A 1 (A 1 (A 2 2)))
    ;; = (A 1 (A 1 (A 1 (A 2 1))))
    ;; = (A 1 (A 1 (A 1 2)))
    ;; = (A 1 (A 1 (A 0 (A 1 1)))))
    ;; = (A 1 (A 1 (A 0 2)))
    ;; = (A 1 (A 1 4))
    ;; = (A 1 (A 0 (A 1 3)))
    ;; = (A 1 (A 0 (A 0 (A 1 2))))
    ;; = (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
    ;; = (A 1 (A 0 (A 0 (A 0 2))))
    ;; = (A 1 (A 0 (A 0 4)))
    ;; = (A 1 (A 0 8))
    ;; = (A 1 16)
    ;; = (A 0 (A 1 15))
    ;; = (A 0 (A 0 (A 1 14)))
    ;; = (A 0 (A 0 (A 0 (A 1 13))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 1 12)))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
    ;; = (A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
    ;; = (A 0 (A 0 (A 0 (A 0 4096))))
    ;; = (A 0 (A 0 (A 0 8192)))
    ;; = (A 0 (A 0 16384))
    ;; = (A 0 32768)
    ;; = 65536

    (is (= 65536 (ch1/A 3 3)))
    ;; > (A 3 3)
    ;; = (A 2 (A 3 2))
    ;; = (A 2 (A 2 (A 3 1)))
    ;; = (A 2 (A 2 2))
    ;; = (A 2 (A 1 (A 2 1)))
    ;; = (A 2 (A 1 2))
    ;; = (A 2 (A 0 (A 1 1)))
    ;; = (A 2 (A 0 2))
    ;; = (A 2 4)
    ;; = 65536
    )

  (testing "f"
    (is (= (* 2 5) (ch1/f 5)))

    (is (= (* 2 6) (ch1/f 6)))

    (is (= (* 2 7) (ch1/f 7)))

    (is (= (* 2 8) (ch1/f 8))))

  (testing "g"
    (is (= (int (Math/pow 2 5)) (ch1/g 5)))

    (is (= (int (Math/pow 2 6)) (ch1/g 6)))

    (is (= (int (Math/pow 2 7)) (ch1/g 7)))

    (is (= (int (Math/pow 2 8)) (ch1/g 8))))

  (testing "h"
    (letfn [(expt [b e]
              (loop [e e
                     a b]
                (if (<= e 1)
                  a
                  (recur (dec e) (* a b)))))
            (tetrate [b e]
              (loop [n e
                     a b]
                (if (<= n 1)
                  a
                  (recur (dec n) (expt b a)))))]
      (is (= 4 (expt 2 2)))

      (is (= 8 (expt 2 3)))

      (is (= 16 (expt 2 4)))

      (is (= 16 (tetrate 2 3)))

      (is (= 65536 (tetrate 2 4)))

      (is (= (tetrate 2 2) (ch1/h 2)))

      (is (= (tetrate 2 3) (ch1/h 3)))
 
      (is (= (tetrate 2 4) (ch1/h 4))))))

(deftest ex11-test
  (testing "recursive-f"
    (is (= 0 (ch1/recursive-f 0)))

    (is (= 1 (ch1/recursive-f 1)))

    (is (= 2 (ch1/recursive-f 2)))

    (is (= 4 (ch1/recursive-f 3)))

    (is (= 11 (ch1/recursive-f 4))))

  (testing "iterative-f"
    (is (= 0 (ch1/iterative-f 0)))

    (is (= 1 (ch1/iterative-f 1)))

    (is (= 2 (ch1/iterative-f 2)))

    (is (= 4 (ch1/iterative-f 3)))

    (is (= 11 (ch1/iterative-f 4)))))

(deftest ex11-test
  (testing "pascal"
    ;;   0 1 2 3 4
    ;; 0 1
    ;; 1 1 1
    ;; 2 1 2 1
    ;; 3 1 3 3 1
    ;; 4 1 4 6 4 1
    (is (= 1 (ch1/pascal 0 0)))

    (is (= 1 (ch1/pascal 1 0)))

    (is (= 1 (ch1/pascal 1 1)))

    (is (= 1 (ch1/pascal 2 0)))

    (is (= 2 (ch1/pascal 2 1)))

    (is (= 1 (ch1/pascal 2 2)))

    (is (= 1 (ch1/pascal 3 0)))

    (is (= 3 (ch1/pascal 3 1)))

    (is (= 3 (ch1/pascal 3 2)))

    (is (= 1 (ch1/pascal 3 3)))

    (is (= 1 (ch1/pascal 4 0)))

    (is (= 4 (ch1/pascal 4 1)))

    (is (= 6 (ch1/pascal 4 2)))

    (is (= 4 (ch1/pascal 4 3)))

    (is (= 1 (ch1/pascal 4 4)))))

(deftest ex14-test
  ;; There are 55 invocations of cc for 11 with 5 denominations.
  ;; Each invocation branches into two calls; one branch exhausting
  ;; the number of coins, the other exhausting the amount. Once the
  ;; coins are exhausted, the coin branch is degenerate and always
  ;; returns immediately, making the amount branch result in ~2 * amount
  ;; calls, or O(A). Thus, both branches are O(A * 2^K),
  ;; where A is the amount & K is the number of distinct kinds of coins.
  (is (= (ch1/cc 11 5)
         (reduce
          (fn [a b] (when (= a b) a))
          (ch1/cc 11 5)
          [(+ (ch1/cc 11 4)
              (ch1/cc -39 5))
           (+ (+ (ch1/cc 11 3)
                 (ch1/cc -4 4))
              0)
           (+ (+ (+ (ch1/cc 11 2)
                    (ch1/cc 1 3))
                 0)
              0)
           (+ (+ (+ (+ (ch1/cc 11 1)
                       (ch1/cc 6 2))
                    (+ (ch1/cc 1 2)
                       (ch1/cc -9 3)))
                 0)
              0)
           (+ (+ (+ (+ (+ (ch1/cc 11 0)
                          (ch1/cc 10 1))
                       (+ (ch1/cc 6 1)
                          (ch1/cc 1 2)))
                    (+ (+ (ch1/cc 1 1)
                          (ch1/cc -4 2))
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ (ch1/cc 10 0)
                             (ch1/cc 9 1)))
                       (+ (+ (ch1/cc 6 0)
                             (ch1/cc 5 1))
                          (+ (ch1/cc 1 1)
                             (ch1/cc -4 2))))
                    (+ (+ (+ (ch1/cc 1 0)
                             (ch1/cc 0 1))
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ (ch1/cc 9 0)
                                (ch1/cc 8 1))))
                       (+ (+ 0
                             (+ (ch1/cc 5 0)
                                (ch1/cc 4 1)))
                          (+ (+ (ch1/cc 1 0)
                                (ch1/cc 0 1))
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ (ch1/cc 8 0)
                                   (ch1/cc 7 1)))))
                       (+ (+ 0
                             (+ 0
                                (+ (ch1/cc 4 0)
                                   (ch1/cc 3 1))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ (ch1/cc 7 0)
                                      (ch1/cc 6 1))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ (ch1/cc 3 0)
                                      (ch1/cc 2 1)))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ (ch1/cc 6 0)
                                         (ch1/cc 5 1)))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ (ch1/cc 2 0)
                                         (ch1/cc 1 1))))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ (ch1/cc 5 0)
                                            (ch1/cc 4 1))))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ (ch1/cc 1 0)
                                            (ch1/cc 0 1)))))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            (+ (ch1/cc 4 0)
                                               (ch1/cc 3 1)))))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            1))))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            (+ 0
                                               (+ (ch1/cc 3 0)
                                                  (ch1/cc 2 1))))))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            1))))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            (+ 0
                                               (+ 0
                                                  (+ (ch1/cc 2 0)
                                                     (ch1/cc 1 1)))))))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            1))))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            (+ 0
                                               (+ 0
                                                  (+ 0
                                                     (+ (ch1/cc 1 0)
                                                        (ch1/cc 0 1))))))))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            1))))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ (+ 0
                          (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            (+ 0
                                               (+ 0
                                                  (+ 0
                                                     (+ 0
                                                        1)))))))))))
                       (+ (+ 0
                             (+ 0
                                (+ 0
                                   (+ 0
                                      (+ 0
                                         (+ 0
                                            1))))))
                          (+ (+ 0
                                1)
                             0)))
                    (+ (+ (+ 0
                             1)
                          0)
                       0))
                 0)
              0)
           (+ (+ (+ (+ 1
                       (+ 1
                          1))
                    1)
                 0)
              0)
           4]))))

(deftest ex15-test
  (is (= 6 ;; ex 1.15.a: sine is called 6 times
         ;; ex 1.15.b: N reduces by a factor of 3 each time, and the
         ;; process gains one stack frame per reduction, which is a
         ;; logarithmic rate; thus it requires O(log N) stack space,
         ;; performing O(log N) steps.
         (+ 1 ;; (sine 12.15)
            1 ;; (p (sine 4.05))
            1 ;; (p (p (sine 1.3499999999999999)))
            1 ;; (p (p (p (sine 0.44999999999999996))))
            1 ;; (p (p (p (p (sine 0.15)))))
            1 ;; (p (p (p (p (p (sine 0.049999999999999996))))))
            )))
  (is (= (ch1/sine 12.15)
         (ch1/p
          (ch1/p
           (ch1/p
            (ch1/p
             (ch1/p
              (ch1/sine 0.049999999999999996)))))))))

(deftest ex16-test
  (dotimes [n 10]
    (dotimes [m 10]
      (is (= (ch1/fast-expt n m) (ch1/fast-expt-iter n m))))))

(deftest ex17-test
  (dotimes [n 10]
    (dotimes [m 10]
      (is (= (* n m) (ch1/fast-* n m))))))

(deftest ex18-test
  (for [n (range 10)
        m (range 10)]
    (is (= (* n m) (ch1/fast-*-iter n m)))))
