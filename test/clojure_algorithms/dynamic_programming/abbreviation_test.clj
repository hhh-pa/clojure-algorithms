(ns clojure-algorithms.dynamic-programming.abbreviation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.abbreviation :as ab]))

(deftest abbr-test
  (testing "abbreviation checks"
    (is (true? (ab/abbr "daBcd" "ABC")))
    (is (false? (ab/abbr "dBcd" "ABC")))
    (is (true? (ab/abbr "ABC" "ABC")))
    (is (true? (ab/abbr "abc" "")))
    (is (false? (ab/abbr "ABC" "")))
    (is (false? (ab/abbr "abC" "ABD")))))
