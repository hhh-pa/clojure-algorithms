(ns clojure-algorithms.sorts.shrink-shell-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.shrink-shell-sort :refer [shell-sort]]))

(deftest test-shell-sort
  (testing "doctest examples"
    (is (= [1 2 3] (shell-sort [3 2 1])))
    (is (= [] (shell-sort [])))
    (is (= [1] (shell-sort [1])))
    (is (= ["apple" "orange" "pear"]
           (shell-sort ["pear" "apple" "orange"])))
    (is (= [-1 0.0 2.5] (shell-sort [2.5 -1 0.0]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 120 #(rand-int 200)))]
      (is (= (vec (sort data)) (shell-sort data))))))
