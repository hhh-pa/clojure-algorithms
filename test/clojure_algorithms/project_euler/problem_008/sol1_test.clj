(ns clojure-algorithms.project-euler.problem-008.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-008.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 609638400 (solution "13978431290823798458352374")))
  (is (= 2612736000 (solution "13978431295823798458352374")))
  (is (= 209018880 (solution "1397843129582379841238352374"))))
