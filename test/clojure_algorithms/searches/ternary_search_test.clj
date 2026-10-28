(ns clojure-algorithms.searches.ternary-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.ternary-search :as ts]))

(deftest lin-search-test
  (testing "bounded linear scan"
    (is (= 3 (ts/lin-search 0 4 [4 5 6 7] 7)))
    (is (= -1 (ts/lin-search 0 3 [4 5 6 7] 7)))
    (is (= 0 (ts/lin-search 0 2 [-18 2] -18)))
    (is (= 0 (ts/lin-search 0 1 [5] 5)))
    (is (= 1 (ts/lin-search 0 3 ["a" "c" "d"] "c")))
    (is (= 0 (ts/lin-search 0 3 [0.1 0.4 -0.1] 0.1)))
    (is (= 2 (ts/lin-search 0 3 [0.1 0.4 -0.1] -0.1)))))

(deftest ite-ternary-search-test
  (testing "iterative ternary search"
    (let [test-list [0 1 2 8 13 17 19 32 42]]
      (is (= -1 (ts/ite-ternary-search test-list 3)))
      (is (= 4 (ts/ite-ternary-search test-list 13))))
    (is (= 0 (ts/ite-ternary-search [4 5 6 7] 4)))
    (is (= -1 (ts/ite-ternary-search [4 5 6 7] -10)))
    (is (= 0 (ts/ite-ternary-search [-18 2] -18)))
    (is (= 0 (ts/ite-ternary-search [5] 5)))
    (is (= 1 (ts/ite-ternary-search ["a" "c" "d"] "c")))
    (is (= -1 (ts/ite-ternary-search ["a" "c" "d"] "f")))
    (is (= -1 (ts/ite-ternary-search [] 1)))
    (is (= 0 (ts/ite-ternary-search [0.1 0.4 -0.1] 0.1)))
    (is (= 65 (ts/ite-ternary-search (vec (range 100)) 65)))
    (is (= -1 (ts/ite-ternary-search (vec (range 100)) 105)))))

(deftest rec-ternary-search-test
  (testing "recursive ternary search"
    (let [test-list [0 1 2 8 13 17 19 32 42]]
      (is (= -1 (ts/rec-ternary-search 0 (count test-list) test-list 3)))
      (is (= 8 (ts/rec-ternary-search 4 (count test-list) test-list 42))))
    (is (= 0 (ts/rec-ternary-search 0 2 [4 5 6 7] 4)))
    (is (= -1 (ts/rec-ternary-search 0 3 [4 5 6 7] -10)))
    (is (= 0 (ts/rec-ternary-search 0 1 [-18 2] -18)))
    (is (= 0 (ts/rec-ternary-search 0 1 [5] 5)))
    (is (= 1 (ts/rec-ternary-search 0 2 ["a" "c" "d"] "c")))
    (is (= -1 (ts/rec-ternary-search 0 2 ["a" "c" "d"] "f")))
    (is (= -1 (ts/rec-ternary-search 0 0 [] 1)))
    (is (= 0 (ts/rec-ternary-search 0 3 [0.1 0.4 -0.1] 0.1)))
    (let [large (vec (range 100))]
      (is (= 65 (ts/rec-ternary-search 0 (count large) large 65)))
      (is (= 65 (ts/rec-ternary-search 20 80 large 65)))
      (is (= -1 (ts/rec-ternary-search 20 80 large 15))))))
