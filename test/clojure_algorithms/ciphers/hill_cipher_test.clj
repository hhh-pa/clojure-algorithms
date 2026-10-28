(ns clojure-algorithms.ciphers.hill-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.hill-cipher :as c]))
(deftest hill-test
  (let [hc (c/make-cipher [[2 5] [1 6]])]
    (is (= 19 (c/replace-letters \T)))
    (is (= 26 (c/replace-letters \0)))
    (is (= \T (c/replace-digits 19)))
    (is (= "TESTINGHILLCIPHERR" (c/process-text hc "Testing Hill Cipher")))
    (is (= "HELLOO" (c/process-text hc "hello")))
    (is (= "WHXYJOLM9C6XT085LL" (c/encrypt hc "testing hill cipher")))
    (is (= "85FF00" (c/encrypt hc "hello")))
    (is (= [[6 25] [5 26]] (c/make-decrypt-key hc)))
    (is (= "TESTINGHILLCIPHERR" (c/decrypt hc "WHXYJOLM9C6XT085LL")))
    (is (= "HELLOO" (c/decrypt hc "85FF00"))))
  (is (thrown? IllegalArgumentException (c/make-cipher [[2 4] [1 2]]))))
