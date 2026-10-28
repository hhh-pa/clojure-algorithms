(ns clojure-algorithms.project-euler.problem-038.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-038.sol1 :refer [pandigital-9? solution]]))

(deftest pandigital-9?-test
  (is (false? (pandigital-9? 12345)))
  (is (true? (pandigital-9? 156284973)))
  (is (false? (pandigital-9? 1562849733))))

(deftest solution-test
  (is (= 932718654 (solution))))
