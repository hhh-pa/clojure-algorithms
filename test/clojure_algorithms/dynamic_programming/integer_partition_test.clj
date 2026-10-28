(ns clojure-algorithms.dynamic-programming.integer-partition-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.integer-partition :as ip]))

(deftest integer-partition-test
  (testing "partition numbers"
    (is (= 7 (ip/partition-count 5)))
    (is (= 15 (ip/partition-count 7)))
    (is (= 190569292 (ip/partition-count 100)))
    (is (= 1 (ip/partition-count 1)))
    (is (= 2 (ip/partition-count 2))))
  (testing "invalid inputs"
    (is (thrown? IndexOutOfBoundsException (ip/partition-count -7)))
    (is (thrown? IndexOutOfBoundsException (ip/partition-count 0)))
    (is (thrown? IllegalArgumentException (ip/partition-count 7.8)))))
