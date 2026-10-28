(ns clojure-algorithms.ciphers.mono-alphabetic-ciphers-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.mono-alphabetic-ciphers :as c]))
(deftest mono-test
  (is (= "Pcssi Bidsm"
         (c/translate-message "QWERTYUIOPASDFGHJKLZXCVBNM" "Hello World" "encrypt")))
  (is (= "Pcssi Bidsm"
         (c/encrypt-message "QWERTYUIOPASDFGHJKLZXCVBNM" "Hello World")))
  (is (= "Itssg Vgksr"
         (c/decrypt-message "QWERTYUIOPASDFGHJKLZXCVBNM" "Hello World"))))
