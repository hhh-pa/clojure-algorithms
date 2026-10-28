(ns clojure-algorithms.ciphers.rot13-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.rot13 :as c]))
(deftest rot13-test
  (let [msg "My secret bank account number is 173-52946 so don't tell anyone!!"]
    (is (= "Zl frperg onax nppbhag ahzore vf 173-52946 fb qba'g gryy nalbar!!"
           (c/dencrypt msg)))
    (is (= msg (c/dencrypt (c/dencrypt msg))))))
