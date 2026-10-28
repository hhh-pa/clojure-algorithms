(ns clojure-algorithms.searches.quick-select-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.quick-select :as qs]))

(deftest quick-select-test
  (testing "0-based order statistics with fixed seeds"
    (is (= 54 (qs/quick-select [2 4 5 7 899 54 32] 5 0)))
    (is (= 4 (qs/quick-select [2 4 5 7 899 54 32] 1 7)))
    (is (= 4 (qs/quick-select [5 4 3 2] 2 3)))
    (is (= 7 (qs/quick-select [3 5 7 10 2 12] 3 11))))
  (testing "out-of-range index yields nil"
    (is (nil? (qs/quick-select [1 2] 5 0)))
    (is (nil? (qs/quick-select [1 2] -1 0))))
  (testing "results are seed-independent"
    (is (= (qs/quick-select [9 3 7 1 8 2 6 4 5] 4 1)
           (qs/quick-select [9 3 7 1 8 2 6 4 5] 4 99)))))

(deftest median-test
  (testing "odd and even medians"
    (is (= 3 (qs/median [3 2 2 9 9] 0)))
    (is (= 6.0 (qs/median [2 2 9 9 9 3] 0)))))
