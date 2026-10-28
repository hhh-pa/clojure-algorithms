(ns clojure-algorithms.machine-learning.apriori-algorithm-test
  (:require [clojure-algorithms.machine-learning.apriori-algorithm :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-fn-load-data-examples
  (is (approx [["milk"] ["milk" "butter"] ["milk" "bread"]
               ["milk" "bread" "chips"]]
              (sut/load-data))))

(deftest test-fn-prune-examples
  (is (approx [["X" "Y"] ["X" "Z"] ["Y" "Z"]]
              (let [itemset ["X" "Y" "Z"]
                    candidates [["X" "Y"] ["X" "Z"] ["Y" "Z"]]]
                (sut/prune itemset candidates 2))))
  (testing "string itemsets below the support threshold are pruned (quarantined)"
    (is (approx [] (let [itemset ["1" "2" "3" "4"]
                         candidates ["1" "2" "4"]]
                     (sut/prune itemset candidates 3))))))

(deftest test-fn-apriori-examples
  (is (approx [[["A" "B"] 1] [["A" "C"] 2] [["B" "C"] 2]]
              (let [data [["A" "B" "C"] ["A" "B"] ["A" "C"] ["A" "D"]
                          ["B" "C"]]]
                (sut/apriori data 2))))
  (is (approx [] (let [data [["1" "2" "3"] ["1" "2"] ["1" "3"] ["1" "4"]
                             ["2" "3"]]]
                   (sut/apriori data 3))))
  (testing "supports are exact when no candidate is pruned away"
    (is (approx [[["a"] 2] [["b"] 1] [["a"] 2]]
                (sut/apriori [["a"] ["b"] ["a"]] 1))))
  (testing "empty result when min-support exceeds every count"
    (is (approx [] (sut/apriori [["a"] ["b"]] 5)))))
