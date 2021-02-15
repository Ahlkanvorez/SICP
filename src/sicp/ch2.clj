(ns sicp.ch2
  (:refer-clojure :exclude [cons])
  (:require [sicp.ch1 :refer [abs average compose fast-expt-iter gcd]]))

(deftype Pair [front back]
  Object
  (toString [this]
    (loop [s (str "(" front)
           p back]
      (if (instance? Pair p)
        (let [back (.-back p)]
          (if (instance? Pair back)
            (recur (str s " " (.-front p)) back)
            (str s " " (.-front p) " " back ")")))
        (str s " . " p ")")))))

(defmethod print-method Pair [pair writer]
  (print-simple (str pair) writer))

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
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(defn width-interval [p]
  (abs (- (upper-bound p) (lower-bound p))))
