(ns clojure-algorithms.other.nested-brackets-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.nested-brackets :as n]))

(deftest balanced-test
  (testing "balanced strings"
    (is (true? (n/balanced? "")))
    (is (true? (n/balanced? "()")))
    (is (true? (n/balanced? "[]")))
    (is (true? (n/balanced? "{}")))
    (is (true? (n/balanced? "()[]{}")))
    (is (true? (n/balanced? "(())")))
    (is (true? (n/balanced? "([{}])")))
    (is (true? (n/balanced? "[[()]]")))
    (is (true? (n/balanced? "(()(()))"))))
  (testing "unbalanced strings"
    (is (false? (n/balanced? "[[")))
    (is (false? (n/balanced? "(()[)]")))
    (is (false? (n/balanced? "([)]")))
    (is (false? (n/balanced? "]"))))
  (testing "non-bracket characters are ignored"
    (is (true? (n/balanced? "Life is a bowl of cherries.")))
    (is (true? (n/balanced? "Life is a bowl of che{}ies.")))
    (is (false? (n/balanced? "Life is a bowl of che}{ies.")))))
