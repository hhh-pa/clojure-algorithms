(ns clojure-algorithms.ciphers.bifid-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.bifid :as c]))
(deftest bifid-test
  (is (= [1 1] (c/letter-to-numbers \a)))
  (is (= [4 5] (c/letter-to-numbers \u)))
  (is (= \u (c/numbers-to-letter 4 5)))
  (is (= "qtltbdxrxlk" (c/encode "testmessage")))
  (is (= "qtltbdxrxlk" (c/encode "Test Message")))
  (is (= (c/encode "test j") (c/encode "test i")))
  (is (= "testmessage" (c/decode "qtltbdxrxlk"))))
