(ns clojure-algorithms.project-euler.problem-014.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-014.sol2
             :refer [collatz-sequence-length solution]]))

(deftest collatz-sequence-length-test
  (is (= 1 (collatz-sequence-length 1)))
  (is (= 10 (collatz-sequence-length 13))))

(deftest solution-test
  (is (= 171 (solution 200)))
  (is (= 3711 (solution 5000))))
