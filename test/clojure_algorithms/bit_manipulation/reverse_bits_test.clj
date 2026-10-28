(ns clojure-algorithms.bit-manipulation.reverse-bits-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.reverse-bits
             :refer [get-reverse-bit-string reverse-bit]]))

(deftest get-reverse-bit-string-test
  (is (= "10010000000000000000000000000000" (get-reverse-bit-string 9)))
  (is (= "11010100000000000000000000000000" (get-reverse-bit-string 43)))
  (is (= "10011100110100000000000000000000" (get-reverse-bit-string 2873)))
  (is (= "00000000000000000000000000011001" (get-reverse-bit-string 2550136832)))
  (is (thrown? clojure.lang.ExceptionInfo (get-reverse-bit-string "this is not a number"))))

(deftest reverse-bit-test
  (testing "doctest examples"
    (is (= 2550136832 (reverse-bit 25)))
    (is (= 2751463424 (reverse-bit 37)))
    (is (= 2818572288 (reverse-bit 21)))
    (is (= 1543503872 (reverse-bit 58)))
    (is (= 0 (reverse-bit 0)))
    (is (= 8388608 (reverse-bit 256)))
    (is (= 25 (reverse-bit 2550136832))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (reverse-bit -1)))
    (is (thrown? clojure.lang.ExceptionInfo (reverse-bit 1.1)))
    (is (thrown? clojure.lang.ExceptionInfo (reverse-bit "0")))))
