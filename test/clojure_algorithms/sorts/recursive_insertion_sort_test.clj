(ns clojure-algorithms.sorts.recursive-insertion-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.recursive-insertion-sort
             :refer [rec-insertion-sort insert-next
                     recursive-insertion-sort]]))

(deftest test-insert-next
  (is (= [2 3 4 2] (insert-next [3 2 4 2] 1)))
  (is (= [3 2 3] (insert-next [3 2 3] 2)))
  (is (= [] (insert-next [] 1))))

(deftest test-rec-insertion-sort
  (testing "doctest examples"
    (is (= [1 1 2] (rec-insertion-sort [1 2 1] 3)))
    (is (= [-2 -1 0 1 2] (rec-insertion-sort [2 1 0 -1 -2] 5)))
    (is (= [1] (rec-insertion-sort [1] 1)))
    (is (= ["a" "b" "c" "d" "e"]
           (rec-insertion-sort ["d" "a" "b" "e" "c"] 5)))
    (is (= [-1 0.0 1 2.5] (rec-insertion-sort [2.5 -1 0.0 1] 4))))
  (testing "full sort matches clojure sort"
    (let [data (vec (repeatedly 60 #(rand-int 100)))]
      (is (= (vec (sort data)) (recursive-insertion-sort data))))))
