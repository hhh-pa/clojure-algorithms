(ns clojure-algorithms.strings.split-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.split :refer [split]]))

(deftest test-split
  (testing "doctest examples"
    (is (= ["apple" "banana" "cherry" "orange"]
           (split "apple#banana#cherry#orange" "#")))
    (is (= ["Hello" "there"] (split "Hello there")))
    (is (= ["11" "22" "63"] (split "11/22/63" "/")))
    (is (= ["12" "43" "39"] (split "12:43:39" ":")))
    (is (= ["" "abbb" "" "c" ""] (split ";abbb;;c;" ";")))
    (is (= ["a" "b" "c"] (split "a--b--c" "--")))
    (is (= ["apple" "banana" "cherry"]
           (split "apple##banana##cherry" "##"))))
  (testing "empty separator"
    (is (= ["abc"] (split "abc" "")))))
