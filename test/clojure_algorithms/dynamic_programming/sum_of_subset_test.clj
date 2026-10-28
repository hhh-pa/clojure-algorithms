(ns clojure-algorithms.dynamic-programming.sum-of-subset-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.sum-of-subset :as ss]))

(deftest sum-subset-test
  (testing "subset sums"
    (is (false? (ss/sum-subset? [2 4 6 8] 5)))
    (is (true? (ss/sum-subset? [2 4 6 8] 14)))
    (is (true? (ss/sum-subset? [2 4 6 8] 0)))
    (is (true? (ss/sum-subset? [3 34 4 12 5 2] 9)))
    (is (false? (ss/sum-subset? [3 34 4 12 5 2] 30)))
    (is (false? (ss/sum-subset? [] 1)))))
