(ns sicp.ch2
  (:refer-clojure :exclude [cons filter list map reverse remove])
  (:require
   [sicp.ch1
    :refer [abs average compose fast-expt-iter fib gcd square prime?]])
  (:import
   [java.awt Color Component Dimension Graphics Graphics2D Image]
   [java.awt.event WindowAdapter WindowEvent]
   [java.awt.geom AffineTransform]
   [java.awt.image BufferedImage]
   [java.io File]
   [javax.imageio ImageIO]
   [javax.swing JFrame]))

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

(defn pair? [p]
  (instance? Pair p))

(defmethod print-method Pair [pair writer]
  (print-simple (str pair) writer))

(def pair? (partial instance? Pair))

(defn cons [front back]
  (->Pair front back))

(defn car [pair]
  (when pair (.-front pair)))

(defn cdr [pair]
  (when pair (.-back pair)))

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
  ([v & coll]
   (loop [accum nil
          coll (vec (conj coll v))]
     (if (seq coll)
       (recur (cons (peek coll) accum) (pop coll))
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

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(defn length [sequence]
  (accumulate (fn [x length] (inc length)) 0 sequence))

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(defn count-leaves [t]
  (accumulate + 0 (map (constantly 1) (fringe t))))

(defn accumulate-n [op init seqs]
  (if (nil? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(defn transpose [mat]
  (accumulate-n cons nil mat))

(defn cons->vec [list]
  (loop [coll []
         list list]
    (if-let [v (car list)]
      (recur (conj coll v) (cdr list))
      coll)))

(defn map
  ([p sequence]
   (accumulate (fn [x y] (cons (p x) y)) nil sequence))
  ([p seq & seqs]
   (let [seqs (cons seq (apply list seqs))]
     (accumulate (fn [seqs accum]
                   (cons (apply p (cons->vec seqs)) accum))
                 nil
                 (transpose seqs)))))

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (fn [row] (dot-product row v)) m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [row] (matrix-*-vector cols row)) m)))

(def fold-right accumulate)

(defn fold-left [op initial sequence]
  (loop [result initial
         rest sequence]
    (if (nil? rest)
      result
      (recur (op result (car rest))
             (cdr rest)))))

(defn reverse-via-foldr [sequence]
  (fold-right (fn [x y] (append y (list x))) nil sequence))

(defn reverse-via-foldl [sequence]
  (fold-left (fn [x y] (cons y x)) nil sequence))

(defn flatmap [proc seq]
  (accumulate append nil (map proc seq)))

(defn prime-sum? [pair]
  (prime? (+ (car pair) (car (cdr pair)))))

(defn make-pair-sum [pair]
  (list (car pair)
        (car (cdr pair))
        (+ (car pair) (car (cdr pair)))))

(defn unique-pairs [n]
  (flatmap (fn [i]
             (map (fn [j] (list i j))
                  (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(defn prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(defn remove [item sequence]
  (filter (fn [x] (not= x item)) sequence))

(defn permutations [s]
  (if (nil? s)
    (list nil)
    (flatmap (fn [x]
               (map (fn [p] (cons x p))
                    (permutations (remove x s))))
             s)))

(defn ordered-triples [n]
  (flatmap (fn [i]
             (map (fn [pair] (cons i pair))
                  (unique-pairs (dec i))))
           (enumerate-interval 1 n)))

(defn triple-sum [triple]
  (accumulate + 0 triple))

(defn triple-sum? [s]
  (fn [triple]
    (= s (triple-sum triple))))

(defn make-triple-sum [triple]
  (list (car triple) (car (cdr triple)) (car (cdr (cdr triple)))
        (triple-sum triple)))

(defn ordered-triples-summing-to [s n]
  (map make-triple-sum (filter (triple-sum? s) (ordered-triples n))))

(defn filter [pred coll]
  (reverse
   (fold-left (fn [accum x]
                (if (pred x)
                  (cons x accum)
                  accum))
              nil
              coll)))

(def empty-board (list))

(defn safe? [k positions]
  (let [pos (car (filter (fn [p] (= k (car (cdr p)))) positions))
        row (car pos)
        col (car (cdr pos))]
    (letfn [(slope-to [p] (/ (- col (car (cdr p))) (- row (car p))))
            (same-row? [p] (= row (car p)))
            (diagonal? [p] (= 1 (abs (slope-to p))))
            (in-check? [p] (or (same-row? p) (diagonal? p)))]
      (nil? (filter in-check? (remove pos positions))))))

(defn adjoin-position [new-row k rest-of-queens]
  (cons (list new-row k) rest-of-queens))

(defn queens [board-size]
  (letfn [(queen-cols [k]
            (if (zero? k)
              (list empty-board)
              (filter
               (fn [positions] (safe? k positions))
               (flatmap
                (fn [rest-of-queens]
                  (map (fn [new-row]
                         (adjoin-position new-row k rest-of-queens))
                       (enumerate-interval 1 board-size)))
                (queen-cols (dec k))))))]
    (queen-cols board-size)))

(defn draw [image]
  (doto (JFrame. "SICP Paintings")
    (.addWindowListener (proxy [WindowAdapter] []
                          (windowClosing [^WindowEvent e]
                            (println :closing))))
    (.add (proxy [Component] []
            (getPreferredSize []
              (Dimension. (.getWidth image nil)
                          (.getHeight image nil)))
            (paint [^Graphics g]
              (.drawImage g image 0 0 nil))))
    (.pack)
    (.setVisible true)))

(defn apply-transform [^AffineTransform f ^BufferedImage v w h]
  (let [^BufferedImage r (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        ^Graphics2D g (.createGraphics r)]
    (.drawImage g v f nil)
    r))

(defn translate-x [^BufferedImage a ^double dx]
  (let [width (.getWidth a nil)
        height (.getHeight a nil)
        new-width (+ width dx)
        result (BufferedImage. new-width height BufferedImage/TYPE_INT_RGB)
        f (AffineTransform/getTranslateInstance dx 0.0)
        scaled-a (apply-transform f a new-width height)]
    (.setData result (.getData scaled-a))
    result))

(defn translate-y [^BufferedImage a ^double dy]
  (let [width (.getWidth a nil)
        height (.getHeight a nil)
        new-height (+ height dy)
        result (BufferedImage. width new-height BufferedImage/TYPE_INT_RGB)
        f (AffineTransform/getTranslateInstance 0.0 dy)
        scaled-a (apply-transform f a width new-height)]
    (.setData result (.getData scaled-a))
    result))

(defn scale-x [^BufferedImage a ^double new-width]
  (let [width (.getWidth a nil)
        height (.getHeight a nil)
        result (BufferedImage. new-width height BufferedImage/TYPE_INT_RGB)
        f (AffineTransform/getScaleInstance (/ new-width width) 1.0)
        scaled-a (apply-transform f a new-width height)]
    (.setData result (.getData scaled-a))
    result))

(defn scale-y [^BufferedImage a ^double new-height]
  (let [width (.getWidth a nil)
        height (.getHeight a nil)
        result (BufferedImage. width new-height BufferedImage/TYPE_INT_RGB)
        f (AffineTransform/getScaleInstance 1.0 (/ new-height height))
        scaled-a (apply-transform f a width new-height)]
    (.setData result (.getData scaled-a))
    result))

(defn reflect-x [^BufferedImage a]
  (let [width (.getWidth a nil)
        height (.getHeight a nil)
        result (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        f (doto (AffineTransform.)
            (.setTransform 1.0  0.0
                           0.0 -1.0
                           0.0 height))
        reflected-a (apply-transform f a width height)]
    (.setData result (.getData reflected-a))
    result))

(defn reflect-y [^BufferedImage a]
  (let [width (.getWidth a nil)
        height (.getHeight a nil)
        result (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        f (doto (AffineTransform.)
            (.setTransform -1.0  0.0
                           0.0   1.0
                           width 0.0))
        reflected-a (apply-transform f a width height)]
    (.setData result (.getData reflected-a))
    result))

(defn rotate [degrees ^BufferedImage a]
  (let [width (.getWidth a nil)
        height (.getHeight a nil)
        result (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        f (AffineTransform/getRotateInstance (Math/toRadians degrees)
                                             (+ (.getMinX a) (/ width 2))
                                             (+ (.getMinY a) (/ height 2)))
        rotated-a (apply-transform f a width height)]
    (.setData result (.getData rotated-a))
    result))

(defn beside [^BufferedImage a ^BufferedImage b]
  (let [w-a (.getWidth a nil)
        w-b (.getWidth b nil)
        h-a (.getHeight a nil)
        h (max h-a (.getHeight b nil))
        result (BufferedImage. (+ w-a w-b) h BufferedImage/TYPE_INT_RGB)
        ^Graphics2D g (.createGraphics result)]
    (.drawImage g (scale-y (translate-x b w-a) h) nil 0 0)
    (.drawImage g (scale-y a h) nil 0 0)
    result))

(defn below [^BufferedImage a ^BufferedImage b]
  (let [w-a (.getWidth a nil)
        w (max w-a (.getWidth b nil))
        h-a (.getHeight a nil)
        h-b (.getHeight b nil)
        result (BufferedImage. w (+ h-a h-b) BufferedImage/TYPE_INT_RGB)
        ^Graphics2D g (.createGraphics result)]
    (.drawImage g (scale-x (translate-y b h-a) w) nil 0 0)
    (.drawImage g (scale-x a w) nil 0 0)
    result))

(defn flip-vert [^BufferedImage painter]
  (let [w (.getWidth painter nil)
        h (.getHeight painter nil)
        result (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        ^Graphics2D g (.createGraphics result)]
    (.drawImage g (reflect-x painter) nil 0 0)
    result))

(defn flip-horiz [^BufferedImage painter]
  (let [w (.getWidth painter nil)
        h (.getHeight painter nil)
        result (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        ^Graphics2D g (.createGraphics result)]
    (.drawImage g (reflect-y painter) nil 0 0)
    result))

(def barton
  (ImageIO/read (File. "resources/ch2/william_barton_rogers.jpg")))

(def barton2 (beside barton barton))
(def barton4 (below barton2 barton2))

(defn flipped-pairs [^BufferedImage painter]
  (let [painter2 (beside painter (flip-vert painter))]
    (below painter2 painter2)))

(defn right-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside (scale-x painter (* 2 (.getWidth painter nil)))
              (below smaller
                     smaller)))))

(defn up-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (up-split painter (dec n))]
      (below (beside smaller smaller)
             (scale-y painter (* 2 (.getHeight painter nil)))))))

(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [right (right-split painter (dec n))
          bottom-right (below right right)
          corner (corner-split painter (dec n))]
      (beside (up-split painter n)
              (below bottom-right corner)))))

(defn square-of-four [tl tr bl br]
  (fn [painter]
    (let [top (beside (tl painter) (tr painter))
          bottom (beside (bl painter) (br painter))]
      (below top bottom))))

(def flipped-pairs-2
  (square-of-four identity flip-vert
                  identity flip-vert))

(defn square-limit [painter n]
  (let [combine (square-of-four flip-horiz identity
                                (partial rotate 180) flip-vert)]
    (combine (corner-split painter n))))

(defn split [primary secondary]
  (fn [painter]
    (primary painter
             (secondary painter painter))))

(def right-split-2 (split beside below))
(def up-split-2 (split below beside))

(def make-frame "'(origin edge1 edge2)" list)
(def origin-frame car)
(def edge1-frame (comp car cdr))
(def edge2-frame (comp car cdr cdr))

(def make-vect "'(x y)" cons)
(def xcor-vect car)
(def ycor-vect cdr)

(defn add-vect [a b]
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(defn sub-vect [x y]
  (add-vect x (scale-vect -1 y)))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(defn make-frame-a [origin edge1 edge2]
  (list origin edge1 edge2))

(def origin-frame-a car)
(def edge1-frame-a (comp car cdr))
(def edge2-frame-a (comp car cdr cdr))

(defn make-frame-b [origin edge1 edge2]
  (cons origin (cons edge1 edge2)))

(def origin-frame-b car)
(def edge1-frame-b (comp car cdr))
(def edge2-frame-b (comp cdr cdr))

(defn draw-line [^Graphics g]
  (fn [start end]
    (.drawLine g
               (xcor-vect start)
               (ycor-vect start)
               (xcor-vect end)
               (ycor-vect end))))

(def make-line-segment cons)
(def start-line-segment car)
(def end-line-segment cdr)

(defn segments->painter [segments ^Graphics g]
  (let [draw-line (draw-line g)]
    (fn [frame]
      (doseq [segment segments]
        (let [frame-coords (frame-coord-map frame)
              start (frame-coords (start-line-segment segment))
              end (frame-coords (end-line-segment segment))]
          (draw-line start end))))))

(defn draw-fn [draw-fn frame]
  (doto (JFrame. "SICP Drawings")
    (.addWindowListener (proxy [WindowAdapter] []
                          (windowClosing [^WindowEvent e]
                            (println :closing))))
    (.add (proxy [Component] []
            (getPreferredSize []
              (Dimension. (xcor-vect (edge1-frame frame))
                          (ycor-vect (edge2-frame frame))))
            (paint [^Graphics g]
              (.setColor g java.awt.Color/RED)
              ((draw-fn g) frame))))
    (.pack)
    (.setVisible true)))

(defn frame-of-size [width height]
  (make-frame (make-vect 0 0)
              (make-vect width 0)
              (make-vect 0 height)))

(def draw-borders
  (partial segments->painter
           [(make-line-segment (make-vect 0 0)
                               (make-vect 1 0))
            (make-line-segment (make-vect 1 0)
                               (make-vect 1 1))
            (make-line-segment (make-vect 1 1)
                               (make-vect 0 1))
            (make-line-segment (make-vect 0 1)
                               (make-vect 0 0))]))

(def draw-x
  (partial segments->painter
           [(make-line-segment (make-vect 0 0)
                               (make-vect 1 1))
            (make-line-segment (make-vect 0 1)
                               (make-vect 1 0))]))

(def draw-diamond
  (partial segments->painter
           [(make-line-segment (make-vect 0 0.5)
                               (make-vect 0.5 0))
            (make-line-segment (make-vect 0.5 0)
                               (make-vect 1 0.5))
            (make-line-segment (make-vect 1 0.5)
                               (make-vect 0.5 1))
            (make-line-segment (make-vect 0.5 1)
                               (make-vect 0 0.5))]))

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))

(defn painter-flip-vert [painter]
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(defn shrink-to-upper-right [painter]
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))

(defn rotate-90 [painter]
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(defn squash-inwards [painter]
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(defn painter-beside [p1 p2]
  (let [split-point (make-vect 0.5 0)
        paint-left (transform-painter p1
                                      (make-vect 0 0)
                                      split-point
                                      (make-vect 0 1))
        paint-right (transform-painter p2
                                       split-point
                                       (make-vect 1 0)
                                       (make-vect 0.5 1))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(defn painter-flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(def rotate-180 (comp rotate-90 rotate-90))
(def rotate-270 (comp rotate-90 rotate-180))

(defn coll->cons [coll]
  (loop [accum nil
         coll coll]
    (if-let [x (first coll)]
      (recur (cons x accum) (rest coll))
      (reverse accum))))

(defn into-cons [form]
  (if (instance? java.util.Collection form)
    (map into-cons (coll->cons form))
    form))

(defn memq [item x]
  (cond (nil? x) false
        (= item (car x)) x
        :else (recur item (cdr x))))

(defmacro scheme-quote [expr]
  `(into-cons '~expr))

;; ex 2.53
;; (list 'a 'b 'c) => '(a b c)
;; (list (list 'george)) => '((george))
;; (cdr '((x1 x2) (y1 y2))) => '((y1 y2))
;; (cadr '((x1 x2) (y1 y2))) => '(y1 y2)
;; (pair? (car '(a short list))) => false
;; (memq 'red '((red shoes) (blue socks))) => false
;; (memq 'red '(red shoes blue socks)) => '(red shoes blue socks)

(defn scheme-equal? [a b]
  (if (and (pair? a) (pair? b))
    (and (= (car a) (car b))
         (scheme-equal? (cdr a) (cdr b)))
    (= a b)))

;; ex 2.55
;; (car ''abracadabra)
;; is syntactic short-hand for
;; (car (quote (quote abracadabra)))
;; which evaluates to the structure
;; (car (list 'quote abracadabra))
;; thus the car is quote.

(defn derivative
  [{:keys
    [variable? same-variable?
     sum? addend augend make-sum
     product? multiplier multiplicand make-product
     exponentiation? base exponent make-exponentiation]}]
  (fn deriv [exp var]
    (cond (number? exp) 0
          (variable? exp) (if (same-variable? exp var) 1 0)
          (sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var))
          (product? exp)
          (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))
          (exponentiation? exp)
          (make-product
           (make-product
            (exponent exp)
            (make-exponentiation (base exp)
                                 (make-sum (exponent exp) -1)))
           (deriv (base exp) var))
          :else (throw (ex-info "Unknown expression type -- DERIV"
                                {:exp exp :var var})))))

(def variable? symbol?)
(defn same-variable? [a b]
  (and (variable? a) (variable? b) (= a b)))

(defn sum? [x] (and (pair? x) (= (car x) '+)))
(def addend (comp car cdr))
(defn augend [sum]
  (let [b (cdr (cdr sum))]
    (if (= 1 (length b))
      (car b)
      (cons '+ b))))

(defn make-sum [a & b]
  (let [b (if (< 1 (count b))
            (apply make-sum (first b) (rest b))
            (first b))]
    (cond (= a 0) b
          (= b 0) a
          (and (number? a) (number? b)) (+ a b)
          :else
          (if (sum? b)
            (cons '+ (cons a (cdr b)))
            (list '+ a b)))))

(defn product? [x] (and (pair? x) (= (car x) '*)))
(def multiplier (comp car cdr))
(defn multiplicand [prod]
  (let [b (cdr (cdr prod))]
    (if (= 1 (length b))
      (car b)
      (cons '* b))))

(defn make-product [a & b]
  (let [b (if (< 1 (count b))
            (apply make-product (first b) (rest b))
            (first b))]
    (cond (or (= a 0) (= b 0)) 0
          (= a 1) b
          (= b 1) a
          (and (number? a) (number? b)) (* a b)
          :else
          (if (product? b)
            (cons '* (cons a (cdr b)))
            (list '* a b)))))

(defn exponentiation? [x] (and (pair? x) (= (car x) '**)))
(def base (comp car cdr))
(def exponent (comp car cdr cdr))

(defn make-exponentiation [b n]
  (cond (= n 0) 1
        (= n 1) b
        (and (number? b) (number? n)) (fast-expt-iter b n)
        :else (list '** b n)))

(def deriv
  (derivative
   {:variable? variable?
    :same-variable? same-variable?
    :make-sum make-sum
    :sum? sum?
    :addend addend
    :augend augend
    :product? product?
    :multiplier multiplier
    :multiplicand multiplicand
    :make-product make-product
    :exponentiation? exponentiation?
    :base base
    :exponent exponent
    :make-exponentiation make-exponentiation}))

(defn infix-sum? [x] (and (pair? x) (= (car (cdr x)) '+)))
(def infix-addend car)
(def infix-augend (comp car cdr cdr))

(defn make-infix-sum [a b]
  (cond (= a 0) b
        (= b 0) a
        (and (number? a) (number? b)) (+ a b)
        :else (list a '+ b)))

(defn infix-product? [x] (and (pair? x) (= (car (cdr x)) '*)))
(def infix-multiplier car)
(def infix-multiplicand (comp car cdr cdr))

(defn make-infix-product [a b]
  (cond (or (= a 0) (= b 0)) 0
        (= a 1) b
        (= b 1) a
        (and (number? a) (number? b)) (* a b)
        :else (list a '* b)))

(defn infix-exponentiation? [x] (and (pair? x) (= (car (cdr x)) '**)))
(def infix-base car)
(def infix-exponent (comp car cdr cdr))

(defn make-infix-exponentiation [b n]
  (cond (= n 0) 1
        (= n 1) b
        (and (number? b) (number? n)) (fast-expt-iter b n)
        :else (list b '** n)))

(def infix-deriv
  (derivative
   {:variable? variable?
    :same-variable? same-variable?
    :make-sum make-infix-sum
    :sum? infix-sum?
    :addend infix-addend
    :augend infix-augend
    :product? infix-product?
    :multiplier infix-multiplier
    :multiplicand infix-multiplicand
    :make-product make-infix-product
    :exponentiation? infix-exponentiation?
    :base infix-base
    :exponent infix-exponent
    :make-exponentiation make-infix-exponentiation}))

(def standard-infix-operator-precedence
  "Order of operations for standard infix; left -> right is
  least -> greatest precedence."
  (scheme-quote (+ * **)))

(defn lowest-precendence-op-in [expression]
  (loop [ops standard-infix-operator-precedence]
    (when-let [op (car ops)]
      (if (nil? (filter (partial = op) expression))
        (recur (cdr ops))
        op))))

(defn parenthesize-standard-infix-expression [expression]
  (let [op (lowest-precendence-op-in expression)]
    (loop [lhs nil
           expression expression]
      (when-let [v (car expression)]
        (if (= op v)
          (let [lhs (if (= (length lhs) 1) (car lhs) (reverse lhs))
                rhs (cdr expression)
                rhs (if (= (length rhs) 1) (car rhs) rhs)]
            (list lhs v rhs))
          (recur (cons v lhs) (cdr expression)))))))

(defn standard-infix-sum? [x]
  (and (pair? x) (= (lowest-precendence-op-in x) '+)))
(def standard-infix-addend
  (comp car parenthesize-standard-infix-expression))
(def standard-infix-augend
  (comp car cdr cdr parenthesize-standard-infix-expression))

(defn standard-infix-product? [x]
  (and (pair? x) (= (lowest-precendence-op-in x) '*)))
(def standard-infix-multiplier
  (comp car parenthesize-standard-infix-expression))
(def standard-infix-multiplicand
  (comp car cdr cdr parenthesize-standard-infix-expression))

(defn standard-infix-exponentiation? [x]
  (and (pair? x) (= (lowest-precendence-op-in x) '**)))
(def standard-infix-base
  (comp car parenthesize-standard-infix-expression))
(def standard-infix-exponent
  (comp car cdr cdr parenthesize-standard-infix-expression))

(def standard-infix-deriv
  (derivative
   {:variable? variable?
    :same-variable? same-variable?
    :make-sum make-infix-sum
    :sum? standard-infix-sum?
    :addend standard-infix-addend
    :augend standard-infix-augend
    :product? standard-infix-product?
    :multiplier standard-infix-multiplier
    :multiplicand standard-infix-multiplicand
    :make-product make-infix-product
    :exponentiation? standard-infix-exponentiation?
    :base standard-infix-base
    :exponent standard-infix-exponent
    :make-exponentiation make-infix-exponentiation}))
