(ns clojure-algorithms.dynamic-programming.longest-common-subsequence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.longest-common-subsequence :as lcs]))

(deftest lcs-test
  (testing "lengths and subsequences"
    (is (= [6 "gaming"] (lcs/longest-common-subsequence "programming" "gaming")))
    (is (= [2 "ph"] (lcs/longest-common-subsequence "physics" "smartphone")))
    (is (= [1 "o"] (lcs/longest-common-subsequence "computer" "food")))
    (is (= [0 ""] (lcs/longest-common-subsequence "" "abc")))
    (is (= [0 ""] (lcs/longest-common-subsequence "abc" "")))
    (is (= [0 ""] (lcs/longest-common-subsequence "" "")))
    (is (= [0 ""] (lcs/longest-common-subsequence "abc" "def")))
    (is (= [3 "abc"] (lcs/longest-common-subsequence "abc" "abc")))
    (is (= [1 "a"] (lcs/longest-common-subsequence "a" "a")))
    (is (= [0 ""] (lcs/longest-common-subsequence "a" "b")))
    (is (= [3 "ace"] (lcs/longest-common-subsequence "abcdef" "ace")))
    (is (= [3 "ABD"] (lcs/longest-common-subsequence "ABCD" "ACBD")))
    (is (= [4 "GTAB"]
           (lcs/longest-common-subsequence "AGGTAB" "GXTXAYB")))))
