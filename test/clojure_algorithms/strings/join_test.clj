(ns clojure-algorithms.strings.join-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.join :as j]))

(deftest test-join
  (testing "doctest examples"
    (is (= "abcd" (j/join "" ["a" "b" "c" "d"])))
    (is (= "a#b#c#d" (j/join "#" ["a" "b" "c" "d"])))
    (is (= "a" (j/join "#" "a")))
    (is (= "You are amazing!" (j/join " " ["You" "are" "amazing!"])))
    (is (= ",," (j/join "," ["" "" ""])))
    (is (= "apple-banana-cherry"
           (j/join "-" ["apple" "banana" "cherry"]))))
  (testing "non-string elements throw"
    (is (thrown? Exception (j/join "#" ["a" "b" "c" 1])))))
