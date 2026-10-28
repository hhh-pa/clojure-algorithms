(ns clojure-algorithms.backtracking.knight-tour-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.knight-tour
             :refer [get-valid-pos complete? open-knight-tour]]))

(deftest get-valid-pos-test
  (is (= [[2 1] [0 1] [3 2]] (get-valid-pos [1 3] 4))))

(deftest complete-test
  (is (true? (complete? [[1]])))
  (is (false? (complete? [[1 2] [3 0]]))))

(deftest open-knight-tour-test
  (is (= [[1]] (open-knight-tour 1)))
  (is (thrown? clojure.lang.ExceptionInfo (open-knight-tour 2)))
  (is (thrown? clojure.lang.ExceptionInfo (open-knight-tour 3))))
