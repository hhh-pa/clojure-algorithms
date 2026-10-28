(ns clojure-algorithms.dynamic-programming.needleman-wunsch-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.needleman-wunsch :as nw]))

(deftest needleman-wunsch-test
  (testing "global alignments"
    (is (= ["GCA-TGCG" "G-ATTACA" 0]
           (nw/needleman-wunsch "GCATGCG" "GATTACA" 1 -1 -1)))
    (is (= ["ACGT" "ACGT" 8]
           (nw/needleman-wunsch "ACGT" "ACGT" 2 -1 -2)))
    (is (= ["AAAA" "TTTT" -4]
           (nw/needleman-wunsch "AAAA" "TTTT" 1 -1 -2)))
    (is (= ["AGTC" "----" -4]
           (nw/needleman-wunsch "AGTC" "" 1 -1 -1)))
    (is (= ["" "" 0] (nw/needleman-wunsch "" "")))
    (is (= ["HEAGAWGHE-E" "---PAW-HEAE" -1]
           (nw/needleman-wunsch "HEAGAWGHEE" "PAWHEAE" 2 -1 -2))))
  (testing "positive gap score is rejected"
    (is (thrown? IllegalArgumentException (nw/needleman-wunsch "A" "C" 1 -1 5)))))
