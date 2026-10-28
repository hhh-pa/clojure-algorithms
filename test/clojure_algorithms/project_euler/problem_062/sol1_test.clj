(ns clojure-algorithms.project-euler.problem-062.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-062.sol1 :refer [get-digits solution]]))

(deftest get-digits-test
  (is (= "27" (get-digits 3)))
  (is (= "027999" (get-digits 99)))
  (is (= "0166788" (get-digits 123))))

(deftest solution-test
  (is (= 125 (solution 2)))
  (is (= 41063625 (solution 3))))
