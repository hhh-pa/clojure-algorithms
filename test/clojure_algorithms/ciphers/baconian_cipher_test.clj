(ns clojure-algorithms.ciphers.baconian-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.baconian-cipher :as c]))
(deftest baconian-test
  (is (= "AABBBAABAAABABAABABAABBAB" (c/encode "hello")))
  (is (= "hello world" (c/decode "AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB")))
  (is (= "hello world" (c/decode (c/encode "hello world"))))
  (is (thrown? Exception (c/encode "hello!")))
  (is (thrown? Exception (c/decode "AAAAA!"))))
