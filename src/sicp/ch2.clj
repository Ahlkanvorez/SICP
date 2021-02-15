(ns sicp.ch2
  (:refer-clojure :exclude [cons])
  (:require [sicp.ch1 :refer [gcd average]]))

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

