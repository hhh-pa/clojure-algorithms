(ns clojure-algorithms.ciphers.trifid-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.trifid-cipher :as c]))
(deftest trifid-test
  (is (= "BCDGBQY" (c/encrypt-message "I am a boy")))
  (is (= "" (c/encrypt-message " ")))
  (is (= "FMJFVOISSUFTFPUFEQQC"
         (c/encrypt-message "   aide toi le c  iel      ta id  era    "
                            "FELIXMARDSTBCGHJKNOPQUVWYZ+" 5)))
  (is (= "IAMABOY" (c/decrypt-message "BCDGBQY")))
  (is (= "AIDETOILECIELTAIDERA"
         (c/decrypt-message "FMJFVOISSUFTFPUFEQQC" "FELIXMARDSTBCGHJKNOPQUVWYZ+" 5))))
