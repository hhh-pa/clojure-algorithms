(ns clojure-algorithms.strings.boyer-moore-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.boyer-moore-search
             :refer [match-in-pattern mismatch-in-text
                     bad-character-heuristic]]))

(deftest test-helpers
  (is (= 1 (match-in-pattern "AB" "B")))
  (is (= 3 (mismatch-in-text "ABAABA" "AB" 2))))

(deftest test-bad-character-heuristic
  (is (= [0 3] (bad-character-heuristic "ABAABA" "AB")))
  (is (= [] (bad-character-heuristic "AAAAA" "AB")))
  (is (= [0 2] (bad-character-heuristic "ABABAB" "ABA")))
  (is (= [] (bad-character-heuristic "" "AB")))
  (is (= [0 1 2 3 4] (bad-character-heuristic "AAAAAA" "AA")))
  (is (= [] (bad-character-heuristic "ABCDEF" "XY"))))
