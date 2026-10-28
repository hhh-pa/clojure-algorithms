(ns clojure-algorithms.ciphers.base58-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.base58 :as c]))
(deftest base58-test
  (is (= "QQ==" (c/base64-encode "A")))
  (is (= "A" (c/base64-decode "QQ==")))
  (is (= "aHR0cHM6Ly90b29scy5pZXRmLm9yZy9odG1sL3JmYzQ2NDg="
         (c/base64-encode "https://tools.ietf.org/html/rfc4648")))
  (is (= "https://tools.ietf.org/html/rfc4648"
         (c/base64-decode "aHR0cHM6Ly90b29scy5pZXRmLm9yZy9odG1sL3JmYzQ2NDg="))))
