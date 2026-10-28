(ns clojure-algorithms.strings.reverse-letters-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.reverse-letters :refer [reverse-letters]]))

(deftest test-reverse-letters
  (testing "doctest examples"
    (is (= "Hey fellow warriors"
           (reverse-letters "Hey wollef sroirraw" 3)))
    (is (= "Python is Python" (reverse-letters "nohtyP is nohtyP" 2)))
    (is (= "1 21 321 4321 12345 123456"
           (reverse-letters "1 12 123 1234 54321 654321" 0)))
    (is (= "racecar" (reverse-letters "racecar")))))
