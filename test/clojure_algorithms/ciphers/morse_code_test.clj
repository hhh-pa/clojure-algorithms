(ns clojure-algorithms.ciphers.morse-code-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.morse-code :as c]))
(deftest morse-test
  (is (= "... --- ... -.-.--" (c/encrypt "Sos!")))
  (is (= (c/encrypt "SOS!") (c/encrypt "sos!")))
  (is (= "SOS!" (c/decrypt "... --- ... -.-.--")))
  (is (= "MORSE CODE HERE!" (c/decrypt (c/encrypt "Morse code here!")))))
