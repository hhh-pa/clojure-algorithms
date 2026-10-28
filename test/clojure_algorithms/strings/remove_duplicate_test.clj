(ns clojure-algorithms.strings.remove-duplicate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.remove-duplicate :refer [remove-duplicates]]))

(deftest test-remove-duplicates
  (testing "doctest examples"
    (is (= "Java Python also and great is"
           (remove-duplicates "Python is great and Java is also great")))
    (is (= "Java Python also and great is"
           (remove-duplicates "Python   is      great and Java is also great")))))
