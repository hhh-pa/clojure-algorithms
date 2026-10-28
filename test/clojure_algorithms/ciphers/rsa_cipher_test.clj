(ns clojure-algorithms.ciphers.rsa-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.rsa-cipher :as c]))
(deftest rsa-test
  (let [p 101 q 103 n (* p q) phi (* (dec p) (dec q)) e 7
        d (.modInverse (biginteger e) (biginteger phi))
        msg "Hi!"]
    (is (= msg (c/decrypt-message (c/encrypt-message msg [n e] 1)
                                  (count msg) [n d] 1)))))
