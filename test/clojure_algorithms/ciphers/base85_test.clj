(ns clojure-algorithms.ciphers.base85-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.base85 :as c]))
(deftest base85-test
  (is (= "" (c/ascii85-encode "")))
  (is (= "0etOA2#" (c/ascii85-encode "12345")))
  (is (= "@UX=h+?24" (c/ascii85-encode "base 85")))
  (is (= "" (c/ascii85-decode "")))
  (is (= "12345" (c/ascii85-decode "0etOA2#")))
  (is (= "base 85" (c/ascii85-decode "@UX=h+?24"))))
