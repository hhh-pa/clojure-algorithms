(ns clojure-algorithms.divide-and-conquer.max-difference-pair-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.max-difference-pair :as md]))

(deftest max-difference-test
  (testing "maximum difference pair"
    (is (= [1 9] (md/max-difference [5 11 2 1 7 9 0 7])))
    (is (= [7 7] (md/max-difference [7])))
    (is (= [1 5] (md/max-difference [1 2 3 4 5])))
    (testing "difference matches brute force"
      (let [coll [9 3 7 1 8 2 6 4]
            [s b] (md/max-difference coll)
            brute (apply max (for [i (range (count coll))
                                   j (range i (count coll))]
                               (- (coll j) (coll i))))]
        (is (= brute (- b s)))))))
