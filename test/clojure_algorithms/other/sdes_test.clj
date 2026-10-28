(ns clojure-algorithms.other.sdes-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.sdes :as s]))

(deftest primitives-test
  (is (= "9012345678" (s/apply-table "0123456789" (range 10))))
  (is (= "1234567890" (s/left-shift "0123456789")))
  (is (= "01011010" (s/xor-bits "01010101" "00001111"))))

(deftest roundtrip-test
  (testing "encrypt then decrypt restores the message"
    (doseq [key ["1010000010" "0000000000" "1111111111"]
            msg ["10101010" "00000000" "11110000"]]
      (is (= msg (s/decrypt key (s/encrypt key msg)))
          (str "key=" key " msg=" msg)))))
