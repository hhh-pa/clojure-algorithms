(ns clojure-algorithms.ciphers.transposition-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.transposition-cipher :as c]))
(deftest transposition-test
  (is (= "Hlia rDsahrij" (c/encrypt-message 6 "Harshil Darji")))
  (is (= "Harshil Darji" (c/decrypt-message 6 "Hlia rDsahrij"))))
