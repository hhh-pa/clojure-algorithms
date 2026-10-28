(ns clojure-algorithms.strings.boyer-moore-horspool-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.boyer-moore-horspool
             :refer [boyer-moore-horspool-search
                     boyer-moore-horspool-search-all build-shift-table]]))

(deftest test-build-shift-table
  (is (= {\a 1 \b 3 \c 2} (build-shift-table "abcab")))
  (is (= {} (build-shift-table "a")))
  (is (= {} (build-shift-table "")))
  (is (= {\a 1} (build-shift-table "aaaa"))))

(deftest test-search
  (is (= 4 (boyer-moore-horspool-search "ABAAABCD" "ABC")))
  (is (= 6 (boyer-moore-horspool-search "hello world" "world")))
  (is (= -1 (boyer-moore-horspool-search "hello world" "Python")))
  (is (= 0 (boyer-moore-horspool-search "aaaaa" "aa")))
  (is (= 0 (boyer-moore-horspool-search "anything" "")))
  (is (= -1 (boyer-moore-horspool-search "" "x"))))

(deftest test-search-all
  (is (= [0 2 5 7] (boyer-moore-horspool-search-all "ababcabab" "ab")))
  (is (= [0 1 2] (boyer-moore-horspool-search-all "aaaa" "aa")))
  (is (= [] (boyer-moore-horspool-search-all "abcdef" "gh")))
  (is (= [0 1 2 3] (boyer-moore-horspool-search-all "abc" "")))
  (is (= [] (boyer-moore-horspool-search-all "" "abc"))))
