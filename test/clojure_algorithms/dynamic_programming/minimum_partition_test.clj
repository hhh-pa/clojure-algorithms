(ns clojure-algorithms.dynamic-programming.minimum-partition-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.minimum-partition :as mp]))

(deftest find-min-test
  (testing "minimum differences"
    (is (= 1 (mp/find-min [1 2 3 4 5])))
    (is (= 5 (mp/find-min [5 5 5 5 5])))
    (is (= 0 (mp/find-min [5 5 5 5])))
    (is (= 3 (mp/find-min [3])))
    (is (= 0 (mp/find-min [])))
    (is (= 0 (mp/find-min [1 2 3 4])))
    (is (= 0 (mp/find-min [0 0 0 0])))
    (is (= 9 (mp/find-min [9 9 9 9 9])))
    (is (= 1 (mp/find-min [1 5 10 3])))
    (is (= 1 (mp/find-min (vec (range 10 0 -1))))))
  (testing "negative inputs are rejected"
    (is (thrown? IndexOutOfBoundsException (mp/find-min [-1])))
    (is (thrown? IndexOutOfBoundsException (mp/find-min [0 0 0 1 2 -4])))))
