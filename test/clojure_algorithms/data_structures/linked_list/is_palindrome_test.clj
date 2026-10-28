(ns clojure-algorithms.data-structures.linked-list.is-palindrome-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.is-palindrome
             :refer [make-node chain-from-vec is-palindrome
                     is-palindrome-stack is-palindrome-dict]]))

(def ^:private cases
  [[nil true]
   [[1] true]
   [[1 2] false]
   [[1 2 1] true]
   [[1 2 2 1] true]
   [[1 2 3] false]
   [[1 2 3 2 1] true]])

(deftest test-palindromes
  (doseq [[items expected] cases]
    (let [head (when items (chain-from-vec items))]
      (is (= expected (is-palindrome head)))
      (is (= expected (is-palindrome-stack head)))
      (is (= expected (is-palindrome-dict head))))))
