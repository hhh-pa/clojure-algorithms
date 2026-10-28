(ns clojure-algorithms.blockchain.simple-blockchain-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.blockchain.simple-blockchain
             :refer [make-block compute-hash make-blockchain add-block valid?]]))

(deftest block-test
  (testing "compute-hash returns a 64-char hex string"
    (let [block (make-block 0 "Genesis" "0" 1)]
      (is (= 64 (count (compute-hash block 0))))
      (is (string? (compute-hash block 0)))))
  (testing "mined hash meets difficulty"
    (is (.startsWith ^String (:hash (make-block 0 "Genesis" "0" 2)) "00")))
  (testing "difficulty below 1 raises"
    (is (thrown? clojure.lang.ExceptionInfo
                 (make-block 0 "Genesis" "0" 0)))))

(deftest blockchain-test
  (testing "genesis block"
    (let [chain (make-blockchain 1)]
      (is (= 0 (get-in chain [:chain 0 :index])))
      (is (.startsWith ^String (get-in chain [:chain 0 :hash]) "0"))))
  (testing "add-block links and validates"
    (let [chain (make-blockchain 1)
          [chain2 block] (add-block chain "Test Data")]
      (is (= 1 (:index block)))
      (is (= (:previous-hash block) (get-in chain [:chain 0 :hash])))
      (is (.startsWith ^String (:hash block) "0"))
      (is (true? (valid? chain2)))))
  (testing "tampering is detected"
    (let [[chain _] (add-block (make-blockchain 1) "Test")]
      (is (true? (valid? chain)))
      (is (false? (valid? (assoc-in chain [:chain 1 :previous-hash] "tampered")))))))
