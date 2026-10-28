(ns clojure-algorithms.ciphers.base32-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.base32 :as c]))
(deftest base32-test
  (is (= "JBSWY3DPEBLW64TMMQQQ====" (c/base32-encode "Hello World!")))
  (is (= "GEZDGNBVGY======" (c/base32-encode "123456")))
  (is (= "Hello World!" (c/base32-decode "JBSWY3DPEBLW64TMMQQQ====")))
  (is (= "123456" (c/base32-decode "GEZDGNBVGY======")))
  (is (= "some long complex string"
         (c/base32-decode (c/base32-encode "some long complex string")))))
