(ns clojure-algorithms.project-euler.problem-067.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-067.sol2 :refer [solution]]))

(def ^:private example-triangle "3\n7 4\n2 4 6\n8 5 9 3")

(deftest solution-test
  (is (= 23 (solution example-triangle)))
  (is (= 7 (solution "7")))
  (is (= 10 (solution "3\n7 4"))))
