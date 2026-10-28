(ns clojure-algorithms.dynamic-programming.wildcard-matching-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.wildcard-matching :as wm]))

(deftest wildcard-match-test
  (testing "wildcard matching"
    (is (true? (wm/wildcard-match? "" "")))
    (is (false? (wm/wildcard-match? "aa" "a")))
    (is (true? (wm/wildcard-match? "abc" "abc")))
    (is (true? (wm/wildcard-match? "abc" "*c")))
    (is (true? (wm/wildcard-match? "abc" "a*")))
    (is (true? (wm/wildcard-match? "abc" "*a*")))
    (is (true? (wm/wildcard-match? "abc" "?b?")))
    (is (true? (wm/wildcard-match? "abc" "*?")))
    (is (false? (wm/wildcard-match? "abc" "a*d")))
    (is (false? (wm/wildcard-match? "abc" "a*c?")))
    (is (false? (wm/wildcard-match? "baaabab" "*****ba*****ba")))
    (is (true? (wm/wildcard-match? "baaabab" "*****ba*****ab")))
    (is (true? (wm/wildcard-match? "aa" "*")))))
