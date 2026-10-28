(ns clojure-algorithms.divide-and-conquer.peak-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.peak :as p]))

(deftest peak-test
  (testing "unimodal peaks"
    (is (= 5 (p/peak [1 2 3 4 5 4 3 2 1])))
    (is (= 10 (p/peak [1 10 9 8 7 6 5 4])))
    (is (= 9 (p/peak [1 9 8 7])))
    (is (= 7 (p/peak [1 2 3 4 5 6 7 0])))
    (is (= 4 (p/peak [1 2 3 4 3 2 1 0 -1 -2])))))
