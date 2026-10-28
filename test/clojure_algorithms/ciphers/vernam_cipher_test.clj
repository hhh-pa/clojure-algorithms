(ns clojure-algorithms.ciphers.vernam-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.vernam-cipher :as c]))
(deftest vernam-test
  (is (= "RIJVS" (c/vernam-encrypt "HELLO" "KEY")))
  (is (= "HELLO" (c/vernam-decrypt "RIJVS" "KEY"))))
