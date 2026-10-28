(ns clojure-algorithms.sorts.insertion-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.insertion-sort :refer [insertion-sort]]))

(deftest test-insertion-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (insertion-sort [0 5 3 2 2])))
    (is (= [] (insertion-sort [])))
    (is (= [-45 -5 -2] (insertion-sort [-2 -5 -45])))
    (is (= (vec (sort ["d" "a" "b" "e" "c"]))
           (insertion-sort ["d" "a" "b" "e" "c"]))))
  (testing "duplicates keep their relative order (stability)"
    (let [tagged (map-indexed (fn [i x] [x i]) [2 1 2 1 2])
          by-first (vec (sort-by first tagged))]
      (is (= [1 1 2 2 2] (insertion-sort (mapv first tagged))))
      (is (= [[1 1] [1 3] [2 0] [2 2] [2 4]] by-first))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 120 #(rand-int 200)))]
      (is (= (vec (sort data)) (insertion-sort data))))))
