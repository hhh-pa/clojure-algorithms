(ns clojure-algorithms.project-euler.problem-044.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-044.sol1 :refer [pentagonal? solution]]))

(deftest pentagonal?-test
  (is (true? (pentagonal? 330)))
  (is (false? (pentagonal? 7683)))
  (is (true? (pentagonal? 2380))))

(deftest solution-test
  (is (= -1 (solution 10))))
