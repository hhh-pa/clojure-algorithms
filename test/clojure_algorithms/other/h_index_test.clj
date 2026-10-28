(ns clojure-algorithms.other.h-index-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.h-index :as h]))

(deftest h-index-test
  (testing "doctest examples"
    (is (= 3 (h/h-index [3 0 6 1 5])))
    (is (= 1 (h/h-index [1 3 1])))
    (is (= 2 (h/h-index [1 2 3]))))
  (testing "invalid inputs throw"
    (is (thrown? IllegalArgumentException (h/h-index "test")))
    (is (thrown? IllegalArgumentException (h/h-index [1 2 "3"])))
    (is (thrown? IllegalArgumentException (h/h-index [1 2 -3]))))
  (testing "empty list"
    (is (= 0 (h/h-index [])))))
