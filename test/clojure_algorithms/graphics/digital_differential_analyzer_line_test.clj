(ns clojure-algorithms.graphics.digital-differential-analyzer-line-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphics.digital-differential-analyzer-line
             :refer [digital-differential-analyzer-line]]))

(deftest test-digital-differential-analyzer-line
  (is (= [[2 2] [3 3] [4 4]]
         (digital-differential-analyzer-line [1 1] [4 4])))
  (is (= [[1 0] [2 0] [3 0]]
         (digital-differential-analyzer-line [0 0] [3 0]))))
