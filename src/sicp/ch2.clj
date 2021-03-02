(ns sicp.ch2
  (:refer-clojure :exclude [cons filter list map reverse])
  (:require
   [sicp.ch1
    :refer [abs average compose fast-expt-iter fib gcd square]]))

(deftype Pair [front back]
  Object
  (toString [this]
    (loop [s (str "(" front)
           p back]
      (if (instance? Pair p)
        (let [back (.-back p)]
          (if (instance? Pair back)
            (recur (str s " " (.-front p)) back)
            (if (nil? back)
              (str s " " (.-front p) ")")
              (str s " " (.-front p) " " back ")"))))
        (if (nil? p)
          (str s ")")
          (str s " . " p ")")))))
  (equals [this other]
    (and (instance? Pair other)
         (= front (.-front other))
         (= back (.-back other)))))

(defmethod print-method Pair [pair writer]
  (print-simple (str pair) writer))

(def pair? (partial instance? Pair))

(defn cons [front back]
  (->Pair front back))

(defn car [pair]
  (.-front pair))

(defn cdr [pair]
  (.-back pair))

(defn make-rat [n d]
  (let [g (gcd n d)]
    (cons (/ n g) (/ d g))))

(def numer car)

(def denom cdr)

(defn format-rat [x]
  (str (numer x) "/" (denom x)))

(def print-rat (comp println format-rat))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def make-point cons)
(def x-point car)
(def y-point cdr)

(defn format-point [p]
  (str "(" (x-point p) ", " (y-point p) ")"))
(def print-point (comp println format-point))

(def make-segment cons)
(def start-segment car)
(def end-segment cdr)

(defn format-segment [s]
  (str "(" (format-point (start-segment s))
       ", " (format-point (end-segment s)) ")"))
(def print-segment (comp println format-segment))

(defn midpoint-segment [line]
  (let [start (start-segment line)
        end (end-segment line)]
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(defn points->rectangle [bottom-left top-left top-right bottom-right]
  (cons bottom-left
        (cons top-left
              (cons top-right
                    bottom-right))))

(defn segments->rectangle [bottom left]
  (let [bottom-left (start-segment bottom)
        bottom-right (end-segment bottom)
        top-left (end-segment left)
        top-right (make-point (x-point bottom-right) (y-point top-left))]
    (points->rectangle bottom-left top-left top-right bottom-right)))

(def make-rectangle points->rectangle)

(def bottom-left-rectangle car)
(def top-left-rectangle (comp car cdr))
(def top-right-rectangle (comp car cdr cdr))
(def bottom-right-rectangle (comp cdr cdr cdr))

(defn top-rectangle [rect]
  (make-segment (top-left-rectangle rect) (top-right-rectangle rect)))

(defn side-rectangle [rect]
  (make-segment (bottom-left-rectangle rect) (top-left-rectangle rect)))

(defn format-rectangle [rect]
  (str "(" (bottom-left-rectangle rect)
       ", " (top-left-rectangle rect)
       ", " (top-right-rectangle rect)
       ", " (bottom-right-rectangle rect)
       ")"))
(def print-rectangle (comp println format-rectangle))

(defn perimeter-rectangle [rect]
  (let [bottom-left (bottom-left-rectangle rect)
        top-left (top-left-rectangle rect)
        top-right (top-right-rectangle rect)
        top-length (- (x-point top-right) (x-point top-left))
        side-length (- (y-point top-left) (y-point bottom-left))]
    (+ (* 2 top-length) (* 2 side-length))))

(defn area-rectangle [rect]
  (let [bottom-left (bottom-left-rectangle rect)
        top-right (top-right-rectangle rect)
        width (- (x-point top-right) (x-point bottom-left))
        height (- (y-point top-right) (y-point bottom-left))]
    (* width height)))

(defn cons-fn [x y] (fn [m] (m x y)))
(defn car-fn [z] (z (fn [p q] p)))
(defn cdr-fn [z] (z (fn [p q] q)))

(defn cons-num [a b]
  (* (fast-expt-iter 2 a) (fast-expt-iter 3 b)))

(defn car-num [p]
  (loop [c 0
         p p]
    (if (zero? (mod p 2))
      (recur (inc c) (quot p 2))
      c)))

(defn cdr-num [p]
  (loop [c 0
         p p]
    (if (zero? (mod p 3))
      (recur (inc c) (quot p 3))
      c)))

(def zero (fn [f] (fn [x] x)))
(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))
(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(defn add-church-numerals [a b]
  (fn [f] (fn [x] (f ((a (b f)) x)))))

(def make-interval cons)
(def lower-bound car)
(def upper-bound cdr)

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn sub-interval [x y]
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (if (<= (lower-bound y) 0 (upper-bound y))
    (throw (ex-info "Cannot divide by interval spanning 0" {:x x :y y}))
    (mul-interval x
                  (make-interval (/ 1 (upper-bound y))
                                 (/ 1 (lower-bound y))))))

(defn width-interval [p]
  (abs (- (upper-bound p) (lower-bound p))))

(defn mul-interval-via-cases [x y]
  (let [a (lower-bound x)
        b (upper-bound x)
        c (lower-bound y)
        d (upper-bound y)]
    (cond
      (and (neg? b) (not (neg? c))) (make-interval (* a d) (* b c))
      (and (neg? d) (not (neg? a))) (make-interval (* b c) (* a d))
      (and (neg? a) (not (neg? b)) (not (neg? c))) (make-interval (* a d) (* b d))
      (and (neg? c) (not (neg? d)) (not (neg? a))) (make-interval (* c b) (* d b))
      (and (neg? b) (neg? c) (not (neg? d))) (make-interval (* a d) (* a c))
      (and (neg? d) (neg? a) (not (neg? b))) (make-interval (* b c) (* a c))
      (and (neg? b) (neg? d)) (make-interval (* b d) (* a c))
      (and (not (neg? a)) (not (neg? c))) (make-interval (* a c) (* b d))
      :else (mul-interval x y))))

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-percent [c p]
  (make-center-width c (* p c)))

(defn percent [i]
  (/ (width i) (center i)))

(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(defn list
  ([] nil)
  ([v] (cons v nil))
  ([v & coll]
   (loop [accum nil
          coll (vec (conj coll v))]
     (if-let [v (peek coll)]
       (recur (cons v accum) (pop coll))
       accum))))

(defn list-ref [items n]
  (if (zero? n)
    (car items)
    (recur (cdr items) (dec n))))

(defn length [items]
  (loop [coll items
         count 0]
    (if (nil? coll)
      count
      (recur (cdr coll) (inc count)))))

(defn append [a b]
  (if (nil? a)
    b
    (cons (car a) (append (cdr a) b))))

(defn last-pair [coll]
  (cond (nil? coll) nil
        (nil? (cdr coll)) coll
        :else (recur (cdr coll))))

(defn reverse [coll]
  (loop [accum nil
         coll coll]
    (if (nil? coll)
      accum
      (recur (cons (car coll) accum) (cdr coll)))))

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(def no-more? nil?)
(def first-denomination car)
(def except-first-denomination cdr)

(defn cc [amount coin-values]
  (cond (zero? amount) 1
        (or (neg? amount) (no-more? coin-values)) 0
        :else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values))))

(defn same-parity [n & coll]
  (let [parity (mod n 2)
        coll (apply list coll)]
    (loop [accum (list n)
           coll coll]
      (if (nil? coll)
        (reverse accum)
        (let [k (car coll)]
          (if (= parity (mod k 2))
            (recur (cons k accum) (cdr coll))
            (recur accum (cdr coll))))))))

(comment
  (defn scale-list [items factor]
    (if (nil? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor)))))

(defn map [proc items]
  (if (nil? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(defn scale-list [items factor]
  (map (fn [x] (* x factor))
       items))

(defn square-list-recursive [items]
  (if (nil? items)
    nil
    (cons (square (car items)) (square-list-recursive (cdr items)))))

(defn square-list-higher-order [items]
  (map square items))

(defn square-list-backwards [items]
  (loop [things items
         answer nil]
    (if (nil? things)
      answer
      (recur (cdr things)
             (cons (square (car things))
                   answer)))))

(defn square-list-inside-out [items]
  (loop [things items
         answer nil]
    (if (nil? things)
      answer
      (recur (cdr things)
             (cons answer
                   (square (car things)))))))

(defn for-each [proc coll]
  (when-not (nil? coll)
    (proc (car coll))
    (recur proc (cdr coll))))

(defn count-leaves [x]
  (cond (nil? x) 0
        (not (pair? x)) 1
        :else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))))

(defn deep-reverse [coll]
  (loop [accum nil
         coll coll]
    (if (nil? coll)
      accum
      (if (pair? (car coll))
        (recur (cons (deep-reverse (car coll)) accum) (cdr coll))
        (recur (cons (car coll) accum) (cdr coll))))))

(defn fringe [coll]
  (if (pair? coll)
    (loop [accum nil
           coll coll]
      (if (nil? coll)
        (reverse accum)
        (let [f (fringe (car coll))]
          (if (pair? f)
            (recur (append (reverse f) accum) (cdr coll))
            (recur (cons f accum) (cdr coll))))))
    coll))

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(def left-branch car)
(def right-branch (compose car cdr))

(def branch-length car)
(def branch-structure (compose car cdr))

(defn total-weight [mobile]
  (cond (pair? mobile)
        (let [left (branch-structure (left-branch mobile))
              right (branch-structure (right-branch mobile))]
          (+ (if (pair? left)
               (total-weight left)
               left)
             (if (pair? right)
               (total-weight right)
               right)))
        (number? mobile) mobile
        :else 0))

(defn torque [branch]
  (if (pair? branch)
    (* (branch-length branch) (total-weight (branch-structure branch)))
    0))

(defn balanced? [mobile]
  (cond (pair? mobile)
        (let [left (left-branch mobile)
              right (right-branch mobile)]
          (and (= (torque left) (torque right))
               (balanced? (branch-structure left))
               (balanced? (branch-structure right))))
        (number? mobile) true
        (nil? mobile) true
        :else false))

(defn make-mobile2 [left right]
  (cons left right))

(defn make-branch2 [length structure]
  (cons length structure))

(def right-branch2 cdr)
(def branch-structure2 cdr)

(defn square-tree [tree]
  (map (fn [tree]
         (if (pair? tree)
           (square-tree tree)
           (square tree)))
       tree))

(defn tree-map [f tree]
  (map (fn [tree]
         (if (pair? tree)
           (tree-map f tree)
           (f tree)))
       tree))

(defn subsets [s]
  (if (nil? s)
    (list (list nil))
    (let [rest (subsets (cdr s))]
      (append rest
              (map (fn [subset]
                     (cond (= subset (list nil)) (list (car s))
                           :else (cons (car s) subset)))
                   rest)))))

(defn filter [predicate sequence]
  (cond (nil? sequence) nil
        (predicate (car sequence))
        (cons (car sequence)
              (filter predicate (cdr sequence)))
        :else (filter predicate (cdr sequence))))


(defn accumulate [op initial sequence]
  (if (nil? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(defn enumerate-interval [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (inc low) high))))

(defn enumerate-tree [tree]
  (cond (nil? tree) nil
        (not (pair? tree)) (list tree)
        :else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))))

(defn sum-odd-squares [tree]
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(defn even-fibs [n]
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(defn list-fib-squares [n]
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(defn product-of-squares-of-odd-elements [sequence]
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(defn map [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) nil sequence))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(defn length [sequence]
  (accumulate (fn [x length] (inc length)) 0 sequence))

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
