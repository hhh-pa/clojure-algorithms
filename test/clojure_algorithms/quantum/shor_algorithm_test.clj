(ns clojure-algorithms.quantum.shor-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.quantum.shor-algorithm :as shor]))

(deftest period-find-test
  (testing "period of a^x mod N"
    (is (= 4 (shor/period-find 2 15)))
    (is (= 6 (shor/period-find 3 7)))))

(deftest shor-algorithm-test
  (testing "even input short-circuits"
    (is (= [2 2] (shor/shor-algorithm 4)))
    (is (= [2 5] (shor/shor-algorithm 10))))
  (testing "factors of 15 multiply back and are non-trivial"
    (is (= [3 5] (shor/shor-algorithm 15)))
    (let [[p q] (shor/shor-algorithm 15)]
      (is (= 15 (* p q)))
      (is (> p 1))
      (is (> q 1))))
  (testing "deterministic for a fixed seed"
    (is (= (shor/shor-algorithm 15 0) (shor/shor-algorithm 15 0)))
    (is (= (shor/shor-algorithm 21 42) (shor/shor-algorithm 21 42)))))
