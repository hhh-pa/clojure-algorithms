(ns clojure-algorithms.conversions.rectangular-to-polar-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.rectangular-to-polar
             :refer [rectangular-to-polar]]))

(deftest test-rectangular-to-polar
  (is (= [7.07 -45.0] (rectangular-to-polar 5 -5)))
  (is (= [1.41 135.0] (rectangular-to-polar -1 1)))
  (is (= [1.41 -135.0] (rectangular-to-polar -1 -1)))
  (is (= [0.0 45.0] (rectangular-to-polar 1e-10 1e-10)))
  (is (= [0.0 135.0] (rectangular-to-polar -1e-10 1e-10)))
  (is (= [11.41 31.31] (rectangular-to-polar 9.75 5.93)))
  (is (= [100497.76 84.29] (rectangular-to-polar 10000 99999))))
