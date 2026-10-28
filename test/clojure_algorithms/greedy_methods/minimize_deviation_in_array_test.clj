(ns clojure-algorithms.greedy-methods.minimize-deviation-in-array-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.minimize-deviation-in-array :refer [minimum-deviation]]))

(deftest test-minimum-deviation
  (is (= 1 (minimum-deviation [1 2 3 4])))
  (is (= 5 (minimum-deviation [5 10 20 30 30])))
  (is (= 0 (minimum-deviation [8 8 8 8 8])))
  (is (= 0 (minimum-deviation [4]))))
