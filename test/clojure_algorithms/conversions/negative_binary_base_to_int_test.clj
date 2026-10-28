(ns clojure-algorithms.conversions.negative-binary-base-to-int-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.negative-binary-base-to-int
             :refer [negative-binary-to-int]]))

(deftest test-negative-binary-to-int
  (is (= -38 (negative-binary-to-int 101110)))
  (is (= -150 (negative-binary-to-int 10111110)))
  (is (= 4 (negative-binary-to-int 100)))
  (is (= 2 (negative-binary-to-int 110))))
