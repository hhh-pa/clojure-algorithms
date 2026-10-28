(ns clojure-algorithms.sorts.topological-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.topological-sort :refer [topological-sort]]))

(deftest test-topological-sort
  (testing "doctest example (bottom-up order)"
    (is (= ["c" "d" "e" "b" "a"] (topological-sort "a" [] []))))
  (testing "top-down order after reversing"
    (is (= ["a" "b" "e" "d" "c"]
           (vec (reverse (topological-sort "a" [] []))))))
  (testing "invalid arguments"
    (is (thrown? IllegalArgumentException (topological-sort "a" "b" "c")))
    (is (thrown? IllegalArgumentException (topological-sort "a" [] "c")))))
