(ns clojure-algorithms.dynamic-programming.word-break-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.word-break :as wb]))

(deftest word-break-test
  (testing "segmentation"
    (is (true? (wb/word-break? "applepenapple" ["apple" "pen"])))
    (is (false? (wb/word-break? "catsandog" ["cats" "dog" "sand" "and" "cat"])))
    (is (true? (wb/word-break? "cars" ["car" "ca" "rs"])))
    (is (false? (wb/word-break? "abc" [])))
    (is (true? (wb/word-break? "aaaa" ["a" "aa"]))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (wb/word-break? 123 ["a"])))
    (is (thrown? IllegalArgumentException (wb/word-break? "" ["a"])))
    (is (thrown? IllegalArgumentException (wb/word-break? "abc" [123])))
    (is (thrown? IllegalArgumentException (wb/word-break? "abc" [""])))))
