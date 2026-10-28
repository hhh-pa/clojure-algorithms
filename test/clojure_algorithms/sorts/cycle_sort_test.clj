(ns clojure-algorithms.sorts.cycle-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.cycle-sort :refer [cycle-sort]]))

(deftest test-cycle-sort
  (testing "doctest examples"
    (is (= [1 2 3 4] (cycle-sort [4 3 2 1])))
    (is (= [-50 -4 -1 0 20 100]
           (cycle-sort [-4 20 0 -50 100 -1])))
    (is (= [-0.8 -0.2 -0.1 1.3] (cycle-sort [-0.1 -0.2 1.3 -0.8])))
    (is (= ["a" "b" "c"] (cycle-sort ["c" "a" "b"])))
    (is (= [] (cycle-sort []))))
  (testing "matches clojure sort on random data"
    (doseq [_ (range 5)]
      (let [data (vec (repeatedly 25 #(rand-int 50)))]
        (is (= (vec (sort data)) (cycle-sort data)))))))
