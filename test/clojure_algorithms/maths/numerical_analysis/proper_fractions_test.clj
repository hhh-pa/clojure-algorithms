(ns clojure-algorithms.maths.numerical-analysis.proper-fractions-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.proper-fractions :as sut]))

(deftest test-fn-proper-fractions-examples
  (is (approx ["1/10" "3/10" "7/10" "9/10"] (sut/proper-fractions 10)))
  (is (approx ["1/5" "2/5" "3/5" "4/5"] (sut/proper-fractions 5)))
  (is (thrown? Exception (sut/proper-fractions -15)))
  (is (approx [] (sut/proper-fractions 0)))
  (is (thrown? Exception (sut/proper-fractions 1.2))))
