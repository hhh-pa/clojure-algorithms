(ns clojure-algorithms.strings.strip-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.strip :refer [strip]]))

(deftest test-strip
  (testing "doctest examples"
    (is (= "hello" (strip "   hello   ")))
    (is (= "world" (strip "...world..." ".")))
    (is (= "hello" (strip "123hello123" "123")))
    (is (= "" (strip "")))))
