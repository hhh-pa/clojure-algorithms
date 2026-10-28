(ns clojure-algorithms.bit-manipulation.gray-code-sequence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.gray-code-sequence
             :refer [gray-code gray-code-sequence-string]]))

(deftest gray-code-test
  (testing "doctest examples"
    (is (= [0] (gray-code 0)))
    (is (= [0 1] (gray-code 1)))
    (is (= [0 1 3 2] (gray-code 2)))
    (is (= [0 1 3 2 6 7 5 4] (gray-code 3))))
  (testing "invalid input"
    (is (thrown? clojure.lang.ExceptionInfo (gray-code -1)))))

(deftest gray-code-sequence-string-test
  (is (= ["0"] (gray-code-sequence-string 0)))
  (is (= ["0" "1"] (gray-code-sequence-string 1)))
  (is (= ["00" "01" "11" "10"] (gray-code-sequence-string 2))))

(deftest gray-code-property-test
  (testing "consecutive codes differ by exactly one bit"
    (let [codes (gray-code 4)
          pairs (partition 2 1 (concat codes [(first codes)]))]
      (is (= 16 (count codes)))
      (is (= 16 (count (distinct codes))))
      (doseq [[a b] pairs]
        (is (= 1 (Long/bitCount (bit-xor a b))))))))
