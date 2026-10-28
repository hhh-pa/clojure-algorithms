(ns clojure-algorithms.project-euler.problem-188.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-188.sol1 :refer [mod-expt
                                                                       solution]]))

(deftest mod-expt-test
  (is (= 6 (mod-expt 2 4 10)))
  (is (= 16 (mod-expt 2 1024 100)))
  (is (= 6 (mod-expt 13 65535 7))))

(deftest solution-test
  (is (= 27 (solution 3 2)))
  (is (= 97484987 (solution 3 3)))
  (is (= 2547 (solution 123 456 4))))
