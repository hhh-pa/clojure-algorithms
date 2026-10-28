(ns clojure-algorithms.sorts.shell-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.shell-sort :refer [shell-sort]]))

(deftest test-shell-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (shell-sort [0 5 3 2 2])))
    (is (= [] (shell-sort [])))
    (is (= [-45 -5 -2] (shell-sort [-2 -5 -45])))
    (is (= ["a" "b" "c"] (shell-sort ["c" "a" "b"])))
    (is (= [-1.0 0.0 2.5] (shell-sort [2.5 -1.0 0.0]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 150 #(rand-int 300)))]
      (is (= (vec (sort data)) (shell-sort data))))))
