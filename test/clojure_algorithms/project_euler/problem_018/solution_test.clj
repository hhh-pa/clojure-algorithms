(ns clojure-algorithms.project-euler.problem-018.solution-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-018.solution :refer [solution]]))

(deftest solution-test
  (is (= 23 (solution "3\n7 4\n2 4 6\n8 5 9 3")))
  (is (= 3 (solution "3"))))
