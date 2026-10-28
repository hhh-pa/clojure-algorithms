(ns clojure-algorithms.ciphers.fractionated-morse-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.fractionated-morse-cipher :as c]))
(deftest fractionated-test
  (is (= "-..x.x..-.x.x-.x-..xx-x....x.xx.x.-x...x-"
         (c/encode-to-morse "defend the east")))
  (is (= "ESOAVVLJRSSTRX"
         (c/encrypt-fractionated-morse "defend the east" "Roundtable")))
  (is (= "DEFEND THE EAST"
         (c/decrypt-fractionated-morse "ESOAVVLJRSSTRX" "Roundtable"))))
