(ns clojure-algorithms.dynamic-programming.smith-waterman-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.smith-waterman :as sw]))

(deftest score-function-test
  (testing "pair scores"
    (is (= 1 (sw/score-function \A \A)))
    (is (= -1 (sw/score-function \A \C)))
    (is (= -2 (sw/score-function \- \A)))
    (is (= -2 (sw/score-function \A \-)))
    (is (= -2 (sw/score-function \- \-)))))

(deftest smith-waterman-test
  (testing "score matrices"
    (is (= [[0 0 0] [0 0 1] [0 1 0] [0 0 2] [0 1 0]]
           (sw/smith-waterman "ACAC" "CA")))
    (is (= [[0 0 0] [0 0 1] [0 1 0] [0 0 2] [0 1 0]]
           (sw/smith-waterman "acac" "ca")))
    (is (= [[0] [0] [0] [0] [0]] (sw/smith-waterman "ACAC" "")))
    (is (= [[0 0 0]] (sw/smith-waterman "" "CA")))
    (is (= [[0 0 0 0] [0 1 0 0] [0 0 2 0] [0 0 0 3]]
           (sw/smith-waterman "AGT" "AGT")))
    (is (= [[0 0 0 0] [0 0 0 1] [0 1 0 0] [0 0 2 0]]
           (sw/smith-waterman "AGT" "GTA")))
    (is (= [[0 0] [0 0] [0 1] [0 0]] (sw/smith-waterman "AGT" "G")))))

(deftest traceback-test
  (testing "traceback alignments"
    (let [score [[0 0 0] [0 0 1] [0 1 0] [0 0 2] [0 1 0]]]
      (is (= "CA\nCA" (sw/traceback score "ACAC" "CA")))
      (is (= "CA\nCA" (sw/traceback score "acac" "ca"))))
    (is (= "" (sw/traceback [[0 0 0]] "ACAC" "")))))
