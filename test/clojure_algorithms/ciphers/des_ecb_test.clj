(ns clojure-algorithms.ciphers.des-ecb-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.des-ecb :as d]))
(deftest ops-test
  (is (= [0 1 0 0 0 0 0 1] (d/string-to-bitset "A")))
  (is (= 16 (count (d/string-to-bitset "ab"))))
  (is (= [1 0 1 0] (d/pad-right-to-multiple-of-n [1 0 1] 4)))
  (is (= [0 1 0 1] (d/pad-left-to-multiple-of-n [1 0 1] 4)))
  (is (= "00000000000000ae"
         (d/bitset-to-hex [1 0 1 0 1 1 1 0]))))
(deftest xor-test
  (is (= [1 1 1 0] (d/xor-bits [0 1 0 1] [1 0 1 1])))
  (is (thrown? IllegalArgumentException (d/xor-bits [1 0] [1]))))
(deftest des-test
  (let [subs (d/key-schedule "133457799BBCDFF1")]
    (is (= 16 (count subs)))
    (is (every? #(= 48 (count %)) subs)))
  (is (= "c84e3c8fb646872720b224896db4f60"
         (d/encrypt "133457799BBCDFF1" "Test string")))
  (is (= "Test string"
         (d/decrypt "133457799BBCDFF1" "c84e3c8fb646872720b224896db4f60"))))
