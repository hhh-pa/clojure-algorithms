(ns clojure-algorithms.neural-network.optimizers.muon-optimizer-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.optimizers.muon-optimizer :as m]))

(deftest muon-test
  (testing "constructor stores momentum"
    (is (= 0.95 (:momentum (m/make-muon 0.02 0.95 5)))))
  (testing "orthogonalize keeps shape"
    (let [orth (m/newton-schulz-orthogonalize [[1.0 0.5] [0.5 1.0]] 5)]
      (is (= [2 2] [(count orth) (count (first orth))]))))
  (testing "orthogonalize handles wide matrices via transpose"
    (let [orth (m/newton-schulz-orthogonalize [[1.0 0.5 0.2]] 2)]
      (is (= [1 3] [(count orth) (count (first orth))]))))
  (testing "update keeps shape"
    (let [o (m/make-muon 0.1 0.9)
          [_ updated] (m/update o 0 [[1.0 2.0] [3.0 4.0]] [[0.1 0.2] [0.3 0.4]])]
      (is (= [2 2] [(count updated) (count (first updated))])))))
