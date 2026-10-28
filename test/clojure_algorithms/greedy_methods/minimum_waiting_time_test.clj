(ns clojure-algorithms.greedy-methods.minimum-waiting-time-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.minimum-waiting-time :refer [minimum-waiting-time]]))

(deftest test-minimum-waiting-time
  (is (= 17 (minimum-waiting-time [3 2 1 2 6])))
  (is (= 4 (minimum-waiting-time [3 2 1])))
  (is (= 10 (minimum-waiting-time [1 2 3 4])))
  (is (= 30 (minimum-waiting-time [5 5 5 5])))
  (is (= 0 (minimum-waiting-time [])))
  (is (= 0 (minimum-waiting-time [7]))))
