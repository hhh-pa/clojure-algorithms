(ns clojure-algorithms.ciphers.polybius-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.polybius :as c]))
(deftest polybius-test
  (is (= [1 1] (c/letter-to-numbers \a)))
  (is (= [4 5] (c/letter-to-numbers \u)))
  (is (= \u (c/numbers-to-letter 4 5)))
  (is (= "44154344 32154343112215" (c/encode "test message")))
  (is (= "44154344 32154343112215" (c/encode "Test Message")))
  (is (= "test message" (c/decode "44154344 32154343112215")))
  (is (= "testmessage" (c/decode "4415434432154343112215"))))
