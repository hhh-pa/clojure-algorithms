(ns clojure-algorithms.strings.palindrome-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.palindrome
             :refer [is-palindrome is-palindrome-traversal
                     is-palindrome-recursive is-palindrome-slice
                     is-palindrome-ignore-case-and-spaces test-data]]))

(deftest test-palindrome-variants
  (doseq [f [is-palindrome is-palindrome-traversal
             is-palindrome-recursive is-palindrome-slice]]
    (testing (str f)
      (doseq [[k v] test-data]
        (is (= v (f k)) k))
      (is (true? (f ""))))))

(deftest test-ignore-case-and-spaces
  (is (true? (is-palindrome-ignore-case-and-spaces
              "A man a plan a canal Panama")))
  (is (true? (is-palindrome-ignore-case-and-spaces
              "Was it a car or a cat I saw?")))
  (is (false? (is-palindrome-ignore-case-and-spaces "Hello World")))
  (is (true? (is-palindrome-ignore-case-and-spaces "Never Odd or Even")))
  (is (true? (is-palindrome-ignore-case-and-spaces "")))
  (is (true? (is-palindrome-ignore-case-and-spaces " !?. ")))
  (is (false? (is-palindrome-ignore-case-and-spaces "1a2"))))
