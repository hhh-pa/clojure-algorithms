(ns clojure-algorithms.blockchain.merkle-tree-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.blockchain.merkle-tree
             :refer [sha256 build-merkle-tree merkle-root verify-proof]]))

(deftest sha256-test
  (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
         (sha256 "abc"))))

(deftest build-merkle-tree-test
  (is (= 64 (count (peek (peek (build-merkle-tree ["a" "b" "c" "d"]))))))
  (is (thrown? clojure.lang.ExceptionInfo (build-merkle-tree [])))
  (testing "odd leaf count duplicates the last node"
    (is (= 64 (count (merkle-root ["a" "b" "c"]))))))

(deftest merkle-root-test
  (is (string? (merkle-root ["tx1" "tx2" "tx3"]))))

(deftest verify-proof-test
  (testing "doctest example"
    (let [data ["a" "b" "c" "d"]
          tree (build-merkle-tree data)
          root (peek (peek tree))
          proof [(sha256 "b") (sha256 (str (sha256 "c") (sha256 "d")))]]
      (is (true? (verify-proof "a" proof root)))
      (is (false? (verify-proof "a" proof "deadbeef")))
      (is (false? (verify-proof "zzz" proof root))))))
