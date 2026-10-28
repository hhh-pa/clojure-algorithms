(ns clojure-algorithms.sorts.stooge-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.stooge-sort :refer [stooge-sort]]))

(deftest test-stooge-sort
  (testing "doctest examples"
    (is (= [-7.1 -1 0 2 2 18.1]
           (stooge-sort [18.1 0 -7.1 -1 2 2])))
    (is (= [] (stooge-sort [])))
    (is (= ["a" "b" "c"] (stooge-sort ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (stooge-sort [2.5 -1 0.0]))))
  (testing "matches clojure sort on small random data"
    (let [data (vec (repeatedly 12 #(rand-int 30)))]
      (is (= (vec (sort data)) (stooge-sort data))))))
