(ns clojure-algorithms.data-compression.burrows-wheeler-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.burrows-wheeler
             :refer [all-rotations bwt-transform reverse-bwt]]))

(deftest test-all-rotations
  (is (= ["^BANANA|" "BANANA|^" "ANANA|^B" "NANA|^BA" "ANA|^BAN" "NA|^BANA"
          "A|^BANAN" "|^BANANA"]
         (all-rotations "^BANANA|")))
  (is (= ["panamabanana" "anamabananap" "namabananapa" "amabananapan"
          "mabananapana" "abananapanam" "bananapanama" "ananapanamab"
          "nanapanamaba" "anapanamaban" "napanamabana" "apanamabanan"]
         (all-rotations "panamabanana")))
  (is (thrown? clojure.lang.ExceptionInfo (all-rotations 5))))

(deftest test-bwt-transform
  (is (= {:bwt-string "BNN^AAA" :idx-original-string 6}
         (bwt-transform "^BANANA")))
  (is (= {:bwt-string "aaaadss_c__aa" :idx-original-string 3}
         (bwt-transform "a_asa_da_casa")))
  (is (= {:bwt-string "mnpbnnaaaaaa" :idx-original-string 11}
         (bwt-transform "panamabanana")))
  (is (thrown? clojure.lang.ExceptionInfo (bwt-transform 4)))
  (is (thrown? clojure.lang.ExceptionInfo (bwt-transform ""))))

(deftest test-reverse-bwt
  (is (= "^BANANA" (reverse-bwt "BNN^AAA" 6)))
  (is (= "a_asa_da_casa" (reverse-bwt "aaaadss_c__aa" 3)))
  (is (= "panamabanana" (reverse-bwt "mnpbnnaaaaaa" 11)))
  (is (= "panamabanana" (reverse-bwt "mnpbnnaaaaaa" 11.0)))
  (is (= "panamabanana" (reverse-bwt "mnpbnnaaaaaa" 11.4)))
  (is (thrown? clojure.lang.ExceptionInfo (reverse-bwt 4 11)))
  (is (thrown? clojure.lang.ExceptionInfo (reverse-bwt "" 11)))
  (is (thrown? clojure.lang.ExceptionInfo (reverse-bwt "mnpbnnaaaaaa" "asd")))
  (is (thrown? clojure.lang.ExceptionInfo (reverse-bwt "mnpbnnaaaaaa" -1)))
  (is (thrown? clojure.lang.ExceptionInfo (reverse-bwt "mnpbnnaaaaaa" 12))))
