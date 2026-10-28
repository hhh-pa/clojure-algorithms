(ns clojure-algorithms.conversions.int-to-negative-binary-base-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.int-to-negative-binary-base
             :refer [decimal-to-negative-binary]]))

(deftest test-decimal-to-negative-binary
  (is (= 11110 (decimal-to-negative-binary 10)))
  (is (= 1010 (decimal-to-negative-binary -10)))
  (is (= 100110 (decimal-to-negative-binary -30)))
  (is (= 1100010 (decimal-to-negative-binary 30)))
  (is (= 0 (decimal-to-negative-binary 0)))
  (is (= 1 (decimal-to-negative-binary 1))))
