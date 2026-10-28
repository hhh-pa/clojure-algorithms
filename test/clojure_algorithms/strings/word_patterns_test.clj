(ns clojure-algorithms.strings.word-patterns-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.word-patterns :refer [get-word-pattern]]))

(deftest test-get-word-pattern
  (testing "doctest examples"
    (is (= "" (get-word-pattern "")))
    (is (= "0" (get-word-pattern " ")))
    (is (= "0.1.2.2.3.4.5" (get-word-pattern "pattern")))
    (is (= "0.1.2.3.4.5.6.7.7.8.2.9" (get-word-pattern "word pattern")))
    (is (= "0.1.2.3.4.5.6.7.3.8.9.2.2.1.6.10"
           (get-word-pattern "get word pattern"))))
  (testing "non-string input throws"
    (doseq [bad [1 1.1 []]]
      (is (thrown? IllegalArgumentException (get-word-pattern bad))
          (str bad)))))
