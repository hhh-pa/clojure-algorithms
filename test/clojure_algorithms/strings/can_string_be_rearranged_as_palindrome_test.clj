(ns clojure-algorithms.strings.can-string-be-rearranged-as-palindrome-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.can-string-be-rearranged-as-palindrome
             :refer [can-string-be-rearranged-as-palindrome
                     can-string-be-rearranged-as-palindrome-counter]]))

(deftest test-both-variants
  (doseq [f [can-string-be-rearranged-as-palindrome
             can-string-be-rearranged-as-palindrome-counter]]
    (testing (str f)
      (is (true? (f "Momo")))
      (is (false? (f "Mother")))
      (is (false? (f "Father")))
      (is (true? (f "A man a plan a canal Panama")))
      (is (true? (f ""))))))
