(ns clojure-algorithms.blockchain.proof-of-stake-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.blockchain.proof-of-stake
             :refer [make-validator choose-validator]]))

(deftest choose-validator-test
  (testing "returns a Validator from the list"
    (let [validators [(make-validator "Alice" 50) (make-validator "Bob" 30)]
          chosen (choose-validator validators)]
      (is (contains? #{"Alice" "Bob"} (:name chosen)))
      (is (contains? #{50 30} (:stake chosen)))))
  (testing "deterministic with a seeded RNG"
    (let [validators [(make-validator "Alice" 50) (make-validator "Bob" 30)]
          a (choose-validator validators (java.util.Random. 42))
          b (choose-validator validators (java.util.Random. 42))]
      (is (= a b))))
  (testing "sole validator is always chosen"
    (let [validators [(make-validator "Solo" 10)]]
      (is (= "Solo" (:name (choose-validator validators (java.util.Random. 1)))))))
  (testing "higher stake wins the statistical majority"
    (let [validators [(make-validator "Whale" 9999) (make-validator "Minnow" 1)]
          rng (java.util.Random. 7)
          picks (repeatedly 200 #(choose-validator validators rng))]
      (is (> (count (filter #(= "Whale" (:name %)) picks)) 150)))))
