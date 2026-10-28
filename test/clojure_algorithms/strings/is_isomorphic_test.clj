(ns clojure-algorithms.strings.is-isomorphic-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-isomorphic :refer [is-isomorphic]]))

(deftest test-is-isomorphic
  (testing "doctest examples"
    (is (true? (is-isomorphic "egg" "add")))
    (is (false? (is-isomorphic "foo" "bar")))
    (is (true? (is-isomorphic "paper" "title")))
    (is (false? (is-isomorphic "ab" "aa")))))
