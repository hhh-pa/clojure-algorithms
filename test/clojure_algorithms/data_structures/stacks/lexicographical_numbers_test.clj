(ns clojure-algorithms.data-structures.stacks.lexicographical-numbers-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.lexicographical-numbers
             :refer [lexical-order]]))

(deftest test-lexical-order
  (is (= "1 10 11 12 13 2 3 4 5 6 7 8 9"
         (clojure.string/join " " (lexical-order 13))))
  (is (= [1] (vec (lexical-order 1))))
  (is (= "1 10 11 12 13 14 15 16 17 18 19 2 20 3 4 5 6 7 8 9"
         (clojure.string/join " " (lexical-order 20))))
  (is (= [1 10 11 12 2 3 4 5 6 7 8 9] (vec (lexical-order 12)))))
