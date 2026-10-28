(ns clojure-algorithms.strings.knuth-morris-pratt-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.knuth-morris-pratt
             :refer [knuth-morris-pratt get-failure-array]]
            [clojure.string :as str]))

(deftest test-knuth-morris-pratt
  (testing "agrees with str/index-of"
    (let [kmp "knuth_morris_pratt"]
      (doseq [s ["kn" "h_m" "rr" "tt" "not there"]]
        (is (= (or (str/index-of kmp s) -1)
               (knuth-morris-pratt kmp s)) s))))
  (testing "reference cases"
    (is (= 14 (knuth-morris-pratt "alskfjaldsabc1abc1abc12k23adsfabcabc"
                                  "abc1abc12")))
    (is (= -1 (knuth-morris-pratt "alskfjaldsk23adsfabcabc" "abc1abc12")))
    (is (= 10 (knuth-morris-pratt "ABABZABABYABABX" "ABABX")))
    (is (= 4 (knuth-morris-pratt "ABAAAAAB" "AAAB")))
    (is (= 15 (knuth-morris-pratt "abcxabcdabxabcdabcdabcy" "abcdabcy")))))

(deftest test-get-failure-array
  (is (= [0 1 0 1 2 3 4 5 2] (get-failure-array "aabaabaaa"))))
