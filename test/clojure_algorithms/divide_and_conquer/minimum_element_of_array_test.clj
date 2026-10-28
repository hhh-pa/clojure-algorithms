(ns clojure-algorithms.divide-and-conquer.minimum-element-of-array-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.minimum-element-of-array :as m]))

(deftest partition-test
  (testing "Lomuto partition"
    (let [[a q] (m/partition-array [-2 3 -10 11 99 100000 100 -200] 0 7)]
      (is (= 0 q))
      (is (= -200 (a 0)))))
  (testing "randomized partition returns a valid pivot"
    (let [arr [-2 3 -10 11 99 100000 100 -200]
          [a q] (m/randomized-partition arr 0 (dec (count arr)))]
      (is (= (sort arr) (sort a)))
      (is (every? #(<= % (a q)) (subvec a 0 q)))
      (is (every? #(> % (a q)) (subvec a (inc q)))))))

(deftest selection-test
  (testing "ith smallest element"
    (let [arr [-2 3 -10 11 99 100000 100 -200]]
      (is (= -200 (m/selection-sort (shuffle arr) 0 7 1)))
      (is (= -200 (m/selection-sort (shuffle arr) 0 7 1)))
      (is (= 100000 (m/selection-sort arr 0 7 8)))
      (is (= (nth (sort arr) 3) (m/selection-sort arr 0 7 4))))
    (is (= -200 (m/selection-sort [-200] 0 0 1)))
    (is (= -2 (m/selection-sort [-2] 0 0 1)))
    (is (= [] (m/selection-sort [] 0 -1 1)))))
