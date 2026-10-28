(ns clojure-algorithms.dynamic-programming.longest-common-substring-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.longest-common-substring :as lcs]))

(deftest longest-common-substring-test
  (testing "common substrings"
    (is (= "" (lcs/longest-common-substring "" "")))
    (is (= "" (lcs/longest-common-substring "a" "")))
    (is (= "" (lcs/longest-common-substring "" "a")))
    (is (= "a" (lcs/longest-common-substring "a" "a")))
    (is (= "bcd" (lcs/longest-common-substring "abcdef" "bcd")))
    (is (= "ab" (lcs/longest-common-substring "abcdef" "xabded")))
    (is (= "Geeks" (lcs/longest-common-substring "GeeksforGeeks" "GeeksQuiz")))
    (is (= "abcd" (lcs/longest-common-substring "abcdxyz" "xyzabcd")))
    (is (= "abcdez" (lcs/longest-common-substring "zxabcdezy" "yzabcdezx")))
    (is (= "Site:Geeks" (lcs/longest-common-substring "OldSite:GeeksforGeeks.org"
                                                      "NewSite:GeeksQuiz.com"))))
  (testing "non-string inputs"
    (is (thrown? IllegalArgumentException (lcs/longest-common-substring 1 1)))))
