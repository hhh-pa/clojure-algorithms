(ns clojure-algorithms.ciphers.base64-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.base64-cipher :as c]))
(deftest base64-test
  (is (= "QQ==" (c/base64-encode "A")))
  (is (= "A" (c/base64-decode "QQ==")))
  (is (= "VGhpcyBwdWxsIHJlcXVlc3QgaXMgcGFydCBvZiBIYWNrdG9iZXJmZXN0MjAh"
         (c/base64-encode "This pull request is part of Hacktoberfest20!")))
  (is (= "This pull request is part of Hacktoberfest20!"
         (c/base64-decode "VGhpcyBwdWxsIHJlcXVlc3QgaXMgcGFydCBvZiBIYWNrdG9iZXJmZXN0MjAh")))
  (is (thrown? AssertionError (c/base64-decode "abc"))))
