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
  (doseq [n (range 10)]
    (doseq [m (range 10)]
      (is (= (* n m) (ch1/fast-*-iter n m))))))

(deftest ex19-test
  (doseq [n (range 92)]
    (is (= (ch1/fib-iter n) (ch1/fib n)))))

(deftest ex20-test
  ;; Applicative order evaluation: 4 remainder operations
  (is (= (ch1/gcd 206 40)
         (ch1/gcd 40 6)   ;; (mod 206 40) => 6
         (ch1/gcd 6 4)    ;; (mod 40 6)   => 4
         (ch1/gcd 4 2)    ;; (mod 6 4)    => 2
         (ch1/gcd 2 0)    ;; (mod 4 2)    => 0
         2))

  ;; Normal order evaluation: 21 remainder operations
  (is (= (ch1/gcd 206 40)
         (ch1/gcd 40 (mod 206 40))
         (ch1/gcd (mod 206 40)
                  (mod 40
                       (mod 206 40)))
         (ch1/gcd (mod 40
                       (mod 206 40))
                  (mod (mod 206 40)
                       (mod 40
                            (mod 206 40))))
         (ch1/gcd (mod (mod 206 40)
                       (mod 40
                            (mod 206 40)))
                  (mod (mod 40
                            (mod 206 40))
                       (mod (mod 206 40)
                            (mod 40
                                 (mod 206 40))))))))

(deftest ex21-test
  (is (= 199 (ch1/smallest-divisor 199)))

  (is (= 1999 (ch1/smallest-divisor 1999)))

  (is (= 7 (ch1/smallest-divisor 19999))))

(deftest ex22-test
  ;; These results suggest prime? runtime is proportional to O(sqrt n),
  ;; and that the runtime in general is proportional to the number of
  ;; computational steps.
  (is
   (<= (Math/abs
        (- (/ (apply min (ch1/prime-runtimes-between 999990 1000010))
              (apply min (ch1/prime-runtimes-between  99990  100010)))
           (Math/sqrt 10)))
       4))

  (is
   (<= (Math/abs
        (- (/ (apply min (ch1/prime-runtimes-between 99990 100010))
              (apply min (ch1/prime-runtimes-between  9990  10010)))
           (Math/sqrt 10)))
       8))

  (is
   (<= (Math/abs
        (- (/ (apply min (ch1/prime-runtimes-between 9990 10010))
              (apply min (ch1/prime-runtimes-between  990  1010)))
           (Math/sqrt 10)))
       16)))

(comment
  (deftest ex23-test
    ;; The tests I ran showed speedups by factors ranging from 0.6 to
    ;; 0.8. However, repeated tests resulted in identical runtimes -- I
    ;; assume the JIT compiler optimized the checks similarly.
    ;; The runtimes are oddly variable, with either version occasionally
    ;; taking 4-8x longer than the other. This can cause the test to
    ;; fail randomly, so it's not really a good test. Because of that,
    ;; I've commented out the block.
    (is (<= (/ (ch1/profile (fn [] (ch1/prime-2? 199)) 100000)
               (ch1/profile (fn [] (ch1/prime? 199)) 100000))
            1))

    (is (<= (/ (ch1/profile (fn [] (ch1/prime-2? 1999)) 100000)
               (ch1/profile (fn [] (ch1/prime? 1999)) 100000))
            1))

    (is (<= (/ (ch1/profile (fn [] (ch1/prime-2? 19999)) 100000)
               (ch1/profile (fn [] (ch1/prime? 19999)) 100000))
            1))))

(deftest ex24-test
  ;; The results suggest fast-prime? runtime is proportional to O(lg n)
  (is
   (<= (Math/abs
        (- (/ (apply min (ch1/fast-prime-runtimes-between 999990 1000010))
              (apply min (ch1/fast-prime-runtimes-between  99990  100010)))
           (Math/log 10)))
       4))

  (is
   (<= (Math/abs
        (- (/ (apply min (ch1/fast-prime-runtimes-between 99990 100010))
              (apply min (ch1/fast-prime-runtimes-between  9990  10010)))
           (Math/log 10)))
       4))

  (is
   (<= (Math/abs
        (- (/ (apply min (ch1/fast-prime-runtimes-between 9990 10010))
              (apply min (ch1/fast-prime-runtimes-between  990  1010)))
           (Math/log 10)))
       4)))

(deftest ex25-test
  ;; The naive method of computing b^e then taking that modulo m will
  ;; result in extremely large integers, overflowing the primitive
  ;; types. While this is not an issue when the numbers are already
  ;; larger than the primitive types, it does mean computations will
  ;; be significantly slower since even ones that can be done with
  ;; only primitives must be promoted to a bigint type.
  (try
    (ch1/bad-expmod 200 200 200)
    (catch ArithmeticException e
      (is (= (.getMessage e) "integer overflow"))))

  (is (= 0 (ch1/exp-mod 200 200 200))))

(deftest ex26-test
  (let [order (ch1/average-slope-order #(ch1/linear-expmod 2 % 7) 25)]
    ;; The order of growth has exponent 1, i.e. O(N); when the input
    ;; size doubles, the runtime also doubles. This is because, while
    ;; the (quot e 2) clause in linear-expmod does logarithmically
    ;; reduce the number of cases to run, that case is run twice since
    ;; it's called twice in the manual * clause (instead of once, were
    ;; the square function used). So, while there are O(log N) steps,
    ;; each step has the pattern for 2^N calls to linear-expmod,
    ;; resulting in O(2^(lg N)) = O(N) calls.
    (is (= 1 order))))

(def carmichael-numbers [561 1105 1729 2465 2821 6601])

(deftest ex27-test
  (doseq [carmichael carmichael-numbers]
    (is (ch1/fast-prime? carmichael 5))

    (is (ch1/mod-prime? carmichael))))

(deftest ex28-test
  (doseq [carmichael carmichael-numbers]
    (is (not (ch1/miller-rabin-prime? carmichael 5))))

  (let [data (range 2 100)]
    (is (= (map ch1/prime? data)
           (map #(ch1/miller-rabin-prime? % 7) data)))))

(deftest ex29-test
  (is (<= (Math/abs (- (ch1/simpsons-rule ch1/cube 0 1 100) 0.25))
          (Math/abs (- (ch1/integral ch1/cube 0 1 0.01) 0.25))))

  (is (<= (Math/abs (- (ch1/simpsons-rule ch1/cube 0 1 1000) 0.25))
          (Math/abs (- (ch1/integral ch1/cube 0 1 0.001) 0.25)))))

(deftest ex30-test
  (is (= (ch1/sum-cubes 1 10)
         (ch1/sum-recursive ch1/cube 1 inc 10))))

(deftest ex31-a-test
  (letfn [(error [n] (Math/abs (- (ch1/approximate-pi n) Math/PI)))]
    (is (< (error 1024)
           (error 512)
           (error 256)
           (error 128)
           (error 64)
           (error 32)
           (error 16)
           (error 8)))))

(deftest ex31-b-test
  (is (= (ch1/factorial 10)
         (ch1/product-recursive identity 1 inc 10))))

(deftest ex32-a-test
  (is (= (ch1/sum-cubes 1 10)
         (ch1/accumulate-sum ch1/cube 1 inc 10)))

  (is (= (ch1/factorial 10)
         (ch1/accumulate-product identity 1 inc 10))))

(deftest ex32-b-test
  (is (= (ch1/accumulate + 0 ch1/cube 1 inc 10)
         (ch1/accumulate-recursive + 0 ch1/cube 1 inc 10)))

  (is (= (ch1/accumulate * 1 identity 1 inc 10)
         (ch1/accumulate-recursive * 1 identity 1 inc 10))))

(deftest ex33-a-test
  (let [N 100]
    (is (= (ch1/sum-prime-squares 1 N)
           (transduce (comp (filter ch1/prime?)
                            (map ch1/square))
                      + 0
                      (range (inc N)))))))

(deftest ex33-b-test
  (let [N 20]
    (is (= (ch1/product-relatively-prime-ints N)
           (transduce (filter (partial ch1/relatively-prime? N))
                      * 1
                      (range 1 (inc N)))))))

(deftest ex34-test
  (letfn [(f [g] (g 2))]
    (is (= 4 (f ch1/square)))

    (is (= 6 (f (fn [z] (* z (inc z))))))

    ;; (f f) = (f 2) = (2 2); 2 is not an IFn, so a ClassCastException
    ;; is thrown.
    (try (f f)
         (catch Throwable e
           (is (= java.lang.ClassCastException (class e)))))))

(deftest ex35-test
  (is (ch1/close-enough? ch1/phi (/ (inc (Math/sqrt 5)) 2))))

(deftest ex36-test
  (is (= "f(5.0000000000) = 4.2920296742
f(4.2920296742) = 4.7418631199
f(4.7418631199) = 4.4382045698
f(4.4382045698) = 4.6352998871
f(4.6352998871) = 4.5039781161
f(4.5039781161) = 4.5899894627
f(4.5899894627) = 4.5330115077
f(4.5330115077) = 4.5704756729
f(4.5704756729) = 4.5457203897
f(4.5457203897) = 4.5620249366
f(4.5620249366) = 4.5512632341
f(4.5512632341) = 4.5583563877
f(4.5583563877) = 4.5536768522
f(4.5536768522) = 4.5567621643
f(4.5567621643) = 4.5547271307
f(4.5547271307) = 4.5560690548
f(4.5560690548) = 4.5551840188
f(4.5551840188) = 4.5557676565
f(4.5557676565) = 4.5553827466
f(4.5553827466) = 4.5556365824
f(4.5556365824) = 4.5554691802
f(4.5554691802) = 4.5555795779
f(4.5555795779) = 4.5555067723
f(4.5555067723) = 4.5555547860
f(4.5555547860) = 4.5555231218
f(4.5555231218) = 4.5555440037
f(4.5555440037) = 4.5555302325
f(4.5555302325) = 4.5555393144
f(4.5555393144) = 4.5555333250
f(4.5555333250) = 4.5555372749
f(4.5555372749) = 4.5555346700
f(4.5555346700) = 4.5555363879
f(4.5555363879) = 4.5555352550
f(4.5555352550) = 4.5555360021
"
         (with-out-str
           (ch1/fixed-point-showing-work
            (fn [x] (/ (Math/log 1000) (Math/log x)))
            5))))
  (is (= 34 (ch1/number-of-steps-for-fixed-point
             (fn [x] (/ (Math/log 1000) (Math/log x)))
             5)))
  ;; Note that with average dampening, the number of steps is
  ;; significantly reduced.
  (is (= 9 (ch1/number-of-steps-for-fixed-point
            (fn [x] (ch1/average x (/ (Math/log 1000) (Math/log x))))
            5))))

(deftest ex37-a-test
  ;; k must be at least 10 to have 4 decimal places of accuracy.
  (letfn [(close-enough? [a b] (< (ch1/abs (- a b)) 0.0001))]
    (is (close-enough? (/ 1 ch1/phi)
                       (ch1/cont-frac (constantly 1) (constantly 1) 10)))))

(deftest ex37-b-test
  (is (= (ch1/cont-frac (constantly 1) (constantly 1) 10)
         (ch1/cont-frac-recursive (constantly 1) (constantly 1) 10))))

(deftest ex38-test
  (is (= Math/E ch1/e)))

(deftest ex39-test
  (letfn [(close-enough? [a b] (< (ch1/abs (- a b)) 0.000000001))
          (tan [x] (double (ch1/tan-cf x 100)))]
    (is (close-enough? (Math/tan 0) (tan 0)))
    (is (close-enough? (Math/tan 1) (tan 1)))
    (is (close-enough? (Math/tan Math/PI) (tan Math/PI)))
    (is (close-enough? (Math/tan (* 2 Math/PI)) (tan (* 2 Math/PI))))))

(deftest ex40-test
  (is (ch1/close-enough? 0 (ch1/newtons-method (ch1/cubic 0 0 0) 1)))

  (is (ch1/close-enough? -2 (ch1/newtons-method (ch1/cubic 0 0 8) 1)))

  (is (ch1/close-enough? -2.1325 (ch1/newtons-method (ch1/cubic 2 3 7) 1))))

(deftest ex41-test
  (is (= 2 ((ch1/double inc) 0)))

  (is (= 13 ((ch1/double inc) 11))))

(deftest ex42-test
  (is (= 49 ((ch1/compose ch1/square inc) 6))))

(deftest ex43-test
  (is (= 625 ((ch1/repeated ch1/square 2) 5)))

  (is (= 10 ((ch1/repeated inc 10) 0)))
  (is (= 11 ((ch1/repeated inc 11) 0)))

  (is (= 625 ((ch1/repeated-fast ch1/square 2) 5)))

  (is (= 10 ((ch1/repeated-fast inc 10) 0)))
  (is (= 11 ((ch1/repeated-fast inc 11) 0))))

(deftest ex44-test
  (is (= 1.0 ((ch1/smooth inc) 0)))

  (is (= (/ (+ (ch1/square (- 2 ch1/dx)) 4 (ch1/square (+ 2 ch1/dx))) 3)
         ((ch1/smooth ch1/square) 2)))

  (is (= ((ch1/smooth (ch1/smooth (ch1/smooth ch1/square))) 2)
         ((ch1/n-smooth ch1/square 3) 2))))

(deftest ex45-test
  (is (ch1/close-enough? 2 (ch1/nth-root 4 2)))

  (is (ch1/close-enough? 2 (ch1/nth-root 8 3)))

  (is (ch1/close-enough? 2 (ch1/nth-root 16 4)))

  (is (ch1/close-enough? 2 (ch1/nth-root 32 5)))

  (is (ch1/close-enough? 2 (ch1/nth-root 64 6)))

  (is (ch1/close-enough? 2 (ch1/nth-root 128 7)))

  (is (ch1/close-enough? 2 (ch1/nth-root 256 8)))

  (is (ch1/close-enough? 2 (ch1/nth-root 512 9)))

  (is (ch1/close-enough? 2 (ch1/nth-root 1024 10))))

(deftest ex46-test
  (doseq [x [2 4 8 16 32 64 128 256 512]]
    (is (ch1/close-enough? (ch1/sqrt-via-average-damp x)
                           (ch1/sqrt-via-iterative-improve x 1.0))))

  (doseq [x [2 4 8 16 32 64 128 256 512]]
    (let [f (fn [y] (ch1/average y (/ x y)))]
      (is (ch1/close-enough?
           (ch1/fixed-point f 1.0)
           (ch1/fixed-point-via-iterative-improve f 1.0))))))
