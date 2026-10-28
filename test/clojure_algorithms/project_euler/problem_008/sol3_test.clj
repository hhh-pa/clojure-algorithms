(ns clojure-algorithms.project-euler.problem-008.sol3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-008.sol3
             :refer [solution str-eval]]))

(deftest str-eval-test
  (is (= 362880 (str-eval "987654321")))
  (is (= 256 (str-eval "22222222"))))

(deftest solution-test
  ;; Value produced by the Python implementation on the full 1000-digit input.
  (is (= 23514624000 (solution))))
