(ns clojure-algorithms.blockchain.simple-proof-of-work-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.blockchain.simple-proof-of-work :refer [proof-of-work]]))

(deftest proof-of-work-test
  (testing "hash meets the difficulty prefix"
    (is (.startsWith ^String (second (proof-of-work 1 "test" "abc" 1)) "0")))
  (testing "same input produces same output"
    (is (= (proof-of-work 1 "data" "hash" 2)
           (proof-of-work 1 "data" "hash" 2))))
  (testing "difficulty 0 returns nonce 0 immediately"
    (is (= 0 (first (proof-of-work 1 "data" "hash" 0)))))
  (testing "negative difficulty raises"
    (is (thrown? clojure.lang.ExceptionInfo (proof-of-work 1 "data" "hash" -1)))))
