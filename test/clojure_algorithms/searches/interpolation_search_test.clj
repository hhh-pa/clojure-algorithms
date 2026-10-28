(ns clojure-algorithms.searches.interpolation-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.interpolation-search :as isearch]))

(deftest interpolation-search-test
  (testing "iterative interpolation search"
    (is (= 1 (isearch/interpolation-search [1 2 3 4 5] 2)))
    (is (= 3 (isearch/interpolation-search [1 2 3 4 5] 4)))
    (is (nil? (isearch/interpolation-search [1 2 3 4 5] 6)))
    (is (nil? (isearch/interpolation-search [] 1)))
    (is (= 0 (isearch/interpolation-search [100] 100)))
    (is (nil? (isearch/interpolation-search [1 2 3 4 5] 0)))
    (is (nil? (isearch/interpolation-search [1 2 3 4 5] 7)))
    (is (nil? (isearch/interpolation-search [5 5 5 5 5] 3)))
    (is (= 0 (isearch/interpolation-search [5 5 5 5 5] 5)))))

(deftest interpolation-search-by-recursion-test
  (testing "recursive interpolation search"
    (is (= 0 (isearch/interpolation-search-by-recursion [0 5 7 10 15] 0)))
    (is (= 4 (isearch/interpolation-search-by-recursion [0 5 7 10 15] 15)))
    (is (= 1 (isearch/interpolation-search-by-recursion [0 5 7 10 15] 5)))
    (is (nil? (isearch/interpolation-search-by-recursion [0 5 7 10 15] 100)))
    (is (nil? (isearch/interpolation-search-by-recursion [0 5 7 10 15] 16)))
    (is (nil? (isearch/interpolation-search-by-recursion [0 5 7 10 15] -1)))
    (is (nil? (isearch/interpolation-search-by-recursion [0 3 6 12 14 15 20]
                                                         10)))
    (is (nil? (isearch/interpolation-search-by-recursion [] 1)))
    (is (nil? (isearch/interpolation-search-by-recursion [5 5 5 5 5] 3)))))
