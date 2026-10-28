(ns clojure-algorithms.sorts.reversort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.reversort :refer [reversort reversort-cost]]))

(deftest test-reversort
  (testing "doctest examples"
    (is (= [1 2 3 4] (reversort [4 2 1 3])))
    (is (= [0 2 2 3 5] (reversort [0 5 3 2 2])))
    (is (= [] (reversort [])))
    (is (= [-45 -5 -2] (reversort [-2 -5 -45])))
    (is (= [1] (reversort [1])))
    (is (= [1 2 3 4 5] (reversort [5 4 3 2 1])))
    (is (= [1 2 3 4] (reversort [2 1 4 3])))
    (is (= [-23 -4 0 6 34] (reversort [-23 0 6 -4 34])))
    (is (= ["a" "b" "c" "x" "y" "z"]
           (reversort ["z" "a" "y" "b" "x" "c"])))
    (is (= [1.1 2.2 3.3 4.4 5.5 6.6 7.7]
           (reversort [1.1 3.3 5.5 7.7 2.2 4.4 6.6]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 60 #(rand-int 100)))]
      (is (= (vec (sort data)) (reversort data))))))

(deftest test-reversort-cost
  (is (= 6 (reversort-cost [4 2 1 3])))
  (is (= 1 (reversort-cost [1 2])))
  (is (= 12 (reversort-cost [7 6 5 4 3 2 1])))
  (is (= 3 (reversort-cost [1 2 3 4])))
  (is (= 0 (reversort-cost [1])))
  (is (= 0 (reversort-cost []))))
