(ns clojure-algorithms.project-euler.problem-493.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-493.sol1 :refer [solution]]))

(deftest solution-test
  (is (= "5.669644129" (solution 10)))
  (is (= "6.985042712" (solution 30))))
