(ns clojure-algorithms.strings.aho-corasick-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.aho-corasick :refer [build-automaton
                                                             search-in]]))

(deftest test-search-in
  (testing "doctest example"
    (let [a (build-automaton ["what" "hat" "ver" "er"])]
      (is (= {"what" [0] "hat" [1] "ver" [5 25] "er" [6 10 22 26]}
             (search-in a "whatever, err ... , wherever")))))
  (testing "no matches and empty text"
    (let [a (build-automaton ["foo" "bar"])]
      (is (= {} (search-in a "no match here")))
      (is (= {} (search-in a "")))))
  (testing "overlapping keywords"
    (let [a (build-automaton ["a" "aa" "aaa"])]
      (is (= {"a" [0 1 2] "aa" [0 1] "aaa" [0]}
             (search-in a "aaa"))))))
