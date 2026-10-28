(ns clojure-algorithms.ciphers.rsa-key-generator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.ciphers.rsa-key-generator :as g]
            [clojure-algorithms.ciphers.rsa-cipher :as c]))
(deftest keygen-test
  (let [[[n1 e] [n2 d]] (g/generate-key 8)
        msg "Hi"]
    (is (= n1 n2))
    (is (= msg (c/decrypt-message (c/encrypt-message msg [n1 e] 1)
                                  (count msg) [n2 d] 1)))))

(deftest keygen-roundtrip-stress-test
  (testing "every generated key roundtrips (guards the p == q flaw)"
    (doseq [_ (range 100)]
      (let [[[n1 e] [n2 d]] (g/generate-key 8)
            msg "Hi"]
        (is (= msg (c/decrypt-message (c/encrypt-message msg [n1 e] 1)
                                      (count msg) [n2 d] 1)))))))
