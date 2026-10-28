(ns clojure-algorithms.divide-and-conquer.inversions-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.inversions :as inv]))

(deftest brute-force-test
  (testing "count-inversions-bf"
    (is (= 4 (inv/count-inversions-bf [1 4 2 4 1])))
    (is (= 0 (inv/count-inversions-bf [1 1 2 4 4])))
    (is (= 0 (inv/count-inversions-bf [])))
    (is (= 8 (inv/count-inversions-bf [10 2 1 5 5 2 11])))))

(deftest cross-inversions-test
  (testing "count-cross-inversions"
    (is (= [[0 1 2 2 3 5] 4]
           (inv/count-cross-inversions [1 2 3] [0 2 5])))
    (is (= [[1 2 3 3 4 5] 0]
           (inv/count-cross-inversions [1 2 3] [3 4 5])))))

(deftest recursive-test
  (testing "count-inversions-recursive"
    (is (= [[1 1 2 4 4] 4]
           (inv/count-inversions-recursive [1 4 2 4 1])))
    (is (= [[1 1 2 4 4] 0]
           (inv/count-inversions-recursive [1 1 2 4 4])))
    (is (= [[] 0] (inv/count-inversions-recursive [])))
    (is (= 8 (second (inv/count-inversions-recursive [10 2 1 5 5 2 11]))))
    (testing "both algorithms agree"
      (doseq [coll [[3 1 2] [5 4 3 2 1] [1 2 3] [2 2 1 1]]]
        (is (= (inv/count-inversions-bf coll)
               (second (inv/count-inversions-recursive coll))))))))
