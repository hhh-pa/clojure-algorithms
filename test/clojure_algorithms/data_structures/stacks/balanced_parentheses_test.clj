(ns clojure-algorithms.data-structures.stacks.balanced-parentheses-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.balanced-parentheses
             :refer [balanced-parentheses]]))

(deftest test-balanced-parentheses
  (is (true? (balanced-parentheses "([]{})")))
  (is (true? (balanced-parentheses "[()]{}{[()()]()}")))
  (is (false? (balanced-parentheses "[(])")))
  (is (true? (balanced-parentheses "1+2*3-4")))
  (is (true? (balanced-parentheses "{}")))
  (is (false? (balanced-parentheses "))((")))
  (is (false? (balanced-parentheses "((")))
  (is (true? (balanced-parentheses "")))
  (is (true? (balanced-parentheses "[[()]]")))
  (is (false? (balanced-parentheses "(")))
  (is (false? (balanced-parentheses ")"))))
