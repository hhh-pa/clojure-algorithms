(ns clojure-algorithms.cellular-automata.elementary-cellular-automaton-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.cellular-automata.elementary-cellular-automaton :as e]))
(deftest rule-30-test
  (is (= [0 1 1 1 0] (e/rule-30-step [0 0 1 0 0])))
  (is (= [0 0 0] (e/rule-30-step [0 0 0]))))
(deftest generate-test
  (is (= 5 (count (e/generate-rule-30 15 5))))
  (is (= 1 (get-in (e/generate-rule-30 15 5) [0 7]))))
