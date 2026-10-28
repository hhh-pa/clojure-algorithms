(ns clojure-algorithms.project-euler.problem-104.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-104.sol1 :refer [check check1]]))

(deftest check-test
  (is (true? (check 123456789987654321)))
  (is (false? (check 120000987654321)))
  (is (true? (check 1234567895765677987654321))))

(deftest check1-test
  (is (true? (check1 123456789987654321)))
  (is (true? (check1 120000987654321)))
  (is (false? (check1 12345678957656779870004321))))
