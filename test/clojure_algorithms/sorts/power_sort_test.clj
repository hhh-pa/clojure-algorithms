(ns clojure-algorithms.sorts.power-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.power-sort :refer [power-sort find-run
                                                         node-power
                                                         merge-runs]]))

(deftest test-helpers
  (is (= [[1 2 3 4 5 6] 3] (find-run [3 2 1 4 5 6] 0 6)))
  (is (= [[1 2 3 2 1] 3] (find-run [1 2 3 2 1] 0 5)))
  (is (= 2 (node-power 100 0 25 25 25)))
  (is (= 1 (node-power 100 0 50 50 50)))
  (is (= [1 2 3 4 5 6] (merge-runs [1 3 5 2 4 6] 0 3 6)))
  (is (= [1 2 3 5 6 7] (merge-runs [5 6 7 1 2 3] 0 3 6))))

(deftest test-power-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (power-sort [0 5 3 2 2])))
    (is (= [] (power-sort [])))
    (is (= [1] (power-sort [1])))
    (is (= [-45 -5 -2] (power-sort [-2 -5 -45])))
    (is (= [1 2 3 4 5] (power-sort [1 2 3 4 5])))
    (is (= [1 2 3 4 5] (power-sort [5 4 3 2 1])))
    (is (= [1 1 2 3 4 5 5 6 9]
           (power-sort [3 1 4 1 5 9 2 6 5])))
    (is (= ["apple" "banana" "cherry"]
           (power-sort ["banana" "apple" "cherry"])))
    (is (= [1.41 1.73 2.71 3.14]
           (power-sort [3.14 2.71 1.41 1.73])))
    (is (= [1 1 2 2 2 3 3 4]
           (power-sort [1 2 3 2 1 2 3 4])))
    (is (= (vec (range 100)) (power-sort (range 100))))
    (is (= (vec (range 50)) (power-sort (reverse (range 50))))))
  (testing "key and reverse"
    (is (= [9 8 5 2 1] (power-sort [5 2 8 1 9] :reverse true)))
    (is (= ["a" "pie" "apple" "longer"]
           (power-sort ["apple" "pie" "a" "longer"] :key count)))
    (is (= ["pie" "longer" "apple" "a"]
           (power-sort ["apple" "pie" "a" "longer"] :reverse true)))
    (is (= [[1 "b"] [1 "a"] [2 "a"]]
           (power-sort [[1 "b"] [2 "a"] [1 "a"]] :key first))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 120 #(rand-int 300)))]
      (is (= (vec (sort data)) (power-sort data))))))
