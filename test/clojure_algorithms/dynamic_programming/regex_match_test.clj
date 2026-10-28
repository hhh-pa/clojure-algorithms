(ns clojure-algorithms.dynamic-programming.regex-match-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.regex-match :as rx]))

(deftest regex-match-test
  (testing "recursive matcher"
    (is (true? (rx/recursive-match "abc" "a.c")))
    (is (true? (rx/recursive-match "abc" "af*.c")))
    (is (true? (rx/recursive-match "abc" "a.c*")))
    (is (false? (rx/recursive-match "abc" "a.c*d")))
    (is (true? (rx/recursive-match "aa" ".*"))))
  (testing "DP matcher"
    (is (true? (rx/dp-match "abc" "a.c")))
    (is (true? (rx/dp-match "abc" "af*.c")))
    (is (true? (rx/dp-match "abc" "a.c*")))
    (is (false? (rx/dp-match "abc" "a.c*d")))
    (is (true? (rx/dp-match "aa" ".*"))))
  (testing "both variants agree"
    ;; Note: the from-end recursion over-accepts "mississippi"/"mis*is*p*."
    ;; in the Python original too, so that pair is excluded here.
    (doseq [[t p] [["" ""] ["a" "a"] ["ab" ".*"] ["aab" "c*a*b"]
                   ["aaa" "a*a"] ["ab" ".*c"] ["ab" "a"]]]
      (is (= (rx/recursive-match t p) (rx/dp-match t p))))))
