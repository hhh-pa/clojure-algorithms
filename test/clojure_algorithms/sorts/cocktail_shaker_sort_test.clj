(ns clojure-algorithms.sorts.cocktail-shaker-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.cocktail-shaker-sort :refer [cocktail-shaker-sort]]))

(deftest test-cocktail-shaker-sort
  (testing "doctest examples"
    (is (= [1 2 2 4 5] (cocktail-shaker-sort [4 5 2 1 2])))
    (is (= [-4 0 1 2 5 11] (cocktail-shaker-sort [-4 5 0 1 2 11])))
    (is (= [-2.4 0.1 2.2 4.4]
           (cocktail-shaker-sort [0.1 -2.4 4.4 2.2])))
    (is (= [1 2 3 4 5] (cocktail-shaker-sort [1 2 3 4 5])))
    (is (= [-24 -11 -7 -5 -4]
           (cocktail-shaker-sort [-4 -5 -24 -7 -11])))
    (is (= ["apple" "banana" "cherry" "date" "elderberry"]
           (cocktail-shaker-sort ["elderberry" "banana" "date" "apple" "cherry"]))))
  (testing "edges"
    (is (= [] (cocktail-shaker-sort [])))
    (is (= [1] (cocktail-shaker-sort [1]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 200)))]
      (is (= (vec (sort data)) (cocktail-shaker-sort data))))))
