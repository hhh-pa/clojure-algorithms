(ns clojure-algorithms.blockchain.proof-of-work-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.blockchain.proof-of-work
             :refer [make-blockchain add-block chain-valid?]]))

(deftest blockchain-test
  (testing "genesis block is mined"
    (let [chain (make-blockchain 1)]
      (is (= 1 (count (:chain chain))))
      (is (= "0" (subs (get-in chain [:chain 0 :hash]) 0 1)))))
  (testing "added blocks link and validate"
    (let [chain (-> (make-blockchain 1)
                    (add-block "Transaction 1: Alice pays Bob 5 BTC")
                    (add-block "Transaction 2: Bob pays Charlie 3 BTC"))]
      (is (= 3 (count (:chain chain))))
      (is (true? (chain-valid? chain)))
      (is (= (get-in chain [:chain 1 :previous-hash])
             (get-in chain [:chain 0 :hash])))))
  (testing "tampering is detected"
    (let [chain (-> (make-blockchain 1)
                    (add-block "Transaction 1: Alice pays Bob 5 BTC"))
          tampered (assoc-in chain [:chain 1 :transactions]
                             "Transaction 1: Alice pays Bob 50 BTC")]
      (is (false? (chain-valid? tampered))))))
