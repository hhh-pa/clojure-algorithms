(ns clojure-algorithms.sorts.bucket-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.bucket-sort :refer [bucket-sort]]))

(def ^:private cases
  [[] [-1 2 -5 0] [9 8 7 6 -12] [0.4 1.2 0.1 0.2 -0.9]
   [-1e10 1e10] [1 2 2 1 1 3] [5 5 5 5 5]
   [1000 -1000 500 -500 0] [5.5 2.2 -1.1 3.3 0.0]
   [-1.1 -1.5 -3.4 2.5 3.6 -3.3] [4 5 3 2 1]
   [0 1 -10 15 2 -2] [1.1 1.2 -1.2 0 2.4]])

(deftest test-bucket-sort
  (testing "matches clojure sort"
    (doseq [c cases]
      (is (= (vec (sort c)) (bucket-sort c)) (str c))))
  (testing "edges"
    (is (= [1] (bucket-sort [1])))
    (is (= [] (bucket-sort [1 2 3] 0))))
  (testing "non-integer bucket count throws"
    (is (thrown? IllegalArgumentException (bucket-sort [1 2 3] 2.5)))))
