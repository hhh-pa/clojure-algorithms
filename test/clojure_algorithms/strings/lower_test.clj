(ns clojure-algorithms.strings.lower-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.lower :refer [lower]]))

(deftest test-lower
  (testing "doctest examples"
    (is (= "wow" (lower "wow")))
    (is (= "hellzo" (lower "HellZo")))
    (is (= "what" (lower "WHAT")))
    (is (= "wh[]32" (lower "wh[]32")))
    (is (= "what" (lower "whAT")))))
