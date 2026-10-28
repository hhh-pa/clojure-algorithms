(ns clojure-algorithms.project-euler.problem-082.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-082.sol1 :refer [solution]]))

(def ^:private example-matrix
  "131,673,234,103,18\n201,96,342,965,150\n630,803,746,422,111\n537,699,497,121,956\n805,732,524,37,331")

(deftest solution-test
  (is (= 3 (solution "1,2\n3,4")))
  (is (= 994 (solution example-matrix))))
