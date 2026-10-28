(ns clojure-algorithms.project-euler.problem-014.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-014.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 171 (solution 200)))
  (is (= 3711 (solution 5000))))
