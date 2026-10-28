(ns clojure-algorithms.ciphers.beaufort-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.beaufort-cipher :as c]))
(deftest beaufort-test
  (is (= "SECRETSECRETSECRE" (c/generate-key "THE GERMAN ATTACK" "SECRET")))
  (is (= "BDC PAYUWL JPAIYI"
         (c/cipher-text "THE GERMAN ATTACK" "SECRETSECRETSECRE")))
  (is (= "THE GERMAN ATTACK"
         (c/original-text "BDC PAYUWL JPAIYI" "SECRETSECRETSECRE"))))
