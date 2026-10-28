(ns clojure-algorithms.ciphers.base16-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.base16 :as c]))
(deftest base16-test
  (is (= "48656C6C6F20576F726C6421" (c/base16-encode "Hello World!")))
  (is (= "" (c/base16-encode "")))
  (is (= "Hello World!" (c/base16-decode "48656C6C6F20576F726C6421")))
  (is (= "" (c/base16-decode "")))
  (is (thrown? IllegalArgumentException (c/base16-decode "486")))
  (is (thrown? IllegalArgumentException (c/base16-decode "zz"))))
