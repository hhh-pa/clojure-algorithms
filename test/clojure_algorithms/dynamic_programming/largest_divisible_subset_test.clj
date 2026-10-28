(ns clojure-algorithms.dynamic-programming.largest-divisible-subset-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.largest-divisible-subset :as ld]))

(deftest largest-divisible-subset-test
  (testing "divisible subsets"
    (is (= [16 8 4 1] (ld/largest-divisible-subset [1 16 7 8 4])))
    (is (= [2 1] (ld/largest-divisible-subset [1 2 3])))
    (is (= [8 4 2 1] (ld/largest-divisible-subset [1 2 4 8])))
    (is (= [1 1 1] (ld/largest-divisible-subset [1 1 1])))
    (is (= [0 0 0] (ld/largest-divisible-subset [0 0 0])))
    (is (= [-1 -1 -1] (ld/largest-divisible-subset [-1 -1 -1])))
    (is (= [] (ld/largest-divisible-subset [])))
    (testing "result is pairwise divisible"
      (let [r (ld/largest-divisible-subset [1 16 7 8 4])]
        (doseq [x r y r]
          (is (or (zero? (mod x y)) (zero? (mod y x)))))))))
