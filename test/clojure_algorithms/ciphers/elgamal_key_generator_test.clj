(ns clojure-algorithms.ciphers.elgamal-key-generator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.elgamal-key-generator :as e]))
(deftest elgamal-test
  (let [g (e/primitive-root 101)]
    (is (<= 3 g 100))
    (is (not= 1 (long (.modPow (biginteger g) (biginteger 2) (biginteger 101))))))
  (let [[[ks e1 e2 p] [_ d]] (e/generate-key 16)]
    (is (= 16 ks))
    (is (pos? p))
    (is (<= 3 d (dec p)))))
