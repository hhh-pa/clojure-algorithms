(ns clojure-algorithms.project-euler.problem-099.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-099.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 2 (solution "2,11\n3,7")))
  (is (= 1 (solution "632382,518061\n519432,525806"))))
