(ns clojure-algorithms.dynamic-programming.iterating-through-submasks-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.iterating-through-submasks :as sm]))

(deftest list-of-submasks-test
  (testing "submask iteration"
    (is (= [15 14 13 12 11 10 9 8 7 6 5 4 3 2 1]
           (sm/list-of-submasks 15)))
    (is (= [13 12 9 8 5 4 1] (sm/list-of-submasks 13)))
    (is (= [8] (sm/list-of-submasks 8)))
    (is (thrown? AssertionError (sm/list-of-submasks -7)))
    (is (thrown? AssertionError (sm/list-of-submasks 0)))))
