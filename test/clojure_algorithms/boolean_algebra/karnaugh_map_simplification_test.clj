(ns clojure-algorithms.boolean-algebra.karnaugh-map-simplification-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.karnaugh-map-simplification :as k]))
(deftest kmap-test
  (is (= "A'B + AB' + AB" (k/simplify-kmap [[0 1] [1 1]])))
  (is (= "" (k/simplify-kmap [[0 0] [0 0]])))
  (is (= "A'B + AB' + AB" (k/simplify-kmap [[0 1] [1 -1]])))
  (is (= "A'B" (k/simplify-kmap [[0 1] [0 0]])))
  (is (= "AB" (k/simplify-kmap [[0 0] [0 1]]))))
