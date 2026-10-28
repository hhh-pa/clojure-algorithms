(ns clojure-algorithms.searches.linear-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.linear-search :as ls]))

(deftest linear-search-test
  (testing "iterative linear search"
    (is (= 0 (ls/linear-search [0 5 7 10 15] 0)))
    (is (= 4 (ls/linear-search [0 5 7 10 15] 15)))
    (is (= 1 (ls/linear-search [0 5 7 10 15] 5)))
    (is (= -1 (ls/linear-search [0 5 7 10 15] 6)))))

(deftest rec-linear-search-test
  (testing "recursive linear search"
    (is (= 0 (ls/rec-linear-search [0 30 500 100 700] 0 4 0)))
    (is (= 4 (ls/rec-linear-search [0 30 500 100 700] 0 4 700)))
    (is (= 1 (ls/rec-linear-search [0 30 500 100 700] 0 4 30)))
    (is (= -1 (ls/rec-linear-search [0 30 500 100 700] 0 4 -6))))
  (testing "invalid bounds throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ls/rec-linear-search [1 2] -1 1 1)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ls/rec-linear-search [1 2] 0 5 1)))))
