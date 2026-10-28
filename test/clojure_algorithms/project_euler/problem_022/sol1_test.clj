(ns clojure-algorithms.project-euler.problem-022.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-022.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 53 (solution "\"COLIN\"")))
  (is (= 164 (solution "\"B\",\"A\",\"COLIN\""))))
