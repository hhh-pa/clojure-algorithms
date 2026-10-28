(ns clojure-algorithms.sorts.iterative-merge-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.iterative-merge-sort :refer [iter-merge-sort]]))

(deftest test-iter-merge-sort
  (testing "doctest examples"
    (is (= [1 2 5 7 7 8 9] (iter-merge-sort [5 9 8 7 1 2 7])))
    (is (= [1] (iter-merge-sort [1])))
    (is (= [1 2] (iter-merge-sort [2 1])))
    (is (= [1 2 3] (iter-merge-sort [2 1 3])))
    (is (= [1 2 3 4] (iter-merge-sort [4 3 2 1])))
    (is (= [1 2 3 4 5] (iter-merge-sort [5 4 3 2 1])))
    (is (= ["a" "b" "c"] (iter-merge-sort ["c" "b" "a"])))
    (is (= [0.1 0.2 0.3] (iter-merge-sort [0.3 0.2 0.1])))
    (is (= ["dang" "dep" "trai"]
           (iter-merge-sort ["dep" "dang" "trai"])))
    (is (= [] (iter-merge-sort [])))
    (is (= [-9 -4 -2 -1] (iter-merge-sort [-2 -9 -1 -4])))
    (is (= [-1.1 -1 0.0 1 1.1]
           (iter-merge-sort [1.1 1 0.0 -1 -1.1]))))
  (testing "matches clojure sort on random data"
    (doseq [n [1 2 3 7 16 33 100]]
      (let [data (vec (repeatedly n #(rand-int 200)))]
        (is (= (vec (sort data)) (iter-merge-sort data)) (str "n=" n))))))
