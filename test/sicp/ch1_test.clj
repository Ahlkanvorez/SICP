(ns sicp.ch1-test
  (:require [clojure.test :refer [deftest is]]
            [sicp.ch1 :as ch1]))

(deftest ex1-test
  (is (= 10 10))

  (is (= 12 (+ 5 3 4)))

  )
