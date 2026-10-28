(ns clojure-algorithms.ciphers.a1z26-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.a1z26 :as c]))
(deftest a1z26-test
  (is (= [13 25 14 1 13 5] (c/encode "myname")))
  (is (= "myname" (c/decode [13 25 14 1 13 5])))
  (is (= "a" (c/decode (c/encode "a"))))
  (is (thrown? IllegalArgumentException (c/encode "abCd")))
  (is (thrown? IllegalArgumentException (c/encode "n0w")))
  (is (thrown? IllegalArgumentException (c/encode "later!"))))
