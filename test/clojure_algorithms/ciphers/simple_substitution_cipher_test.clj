(ns clojure-algorithms.ciphers.simple-substitution-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.simple-substitution-cipher :as c]))
(deftest substitution-test
  (c/check-valid-key "LFWOAYUISVKMNXPBDCRJTQEGHZ")
  (is (thrown? Exception (c/check-valid-key "INVALIDKEY")))
  (is (= "Ilcrism Olcvs"
         (c/encrypt-message "LFWOAYUISVKMNXPBDCRJTQEGHZ" "Harshil Darji")))
  (is (= "Harshil Darji"
         (c/decrypt-message "LFWOAYUISVKMNXPBDCRJTQEGHZ" "Ilcrism Olcvs")))
  (is (= 26 (count (c/get-random-key)))))
