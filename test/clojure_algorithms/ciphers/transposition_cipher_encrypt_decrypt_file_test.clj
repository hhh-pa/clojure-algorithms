(ns clojure-algorithms.ciphers.transposition-cipher-encrypt-decrypt-file-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.transposition-cipher-encrypt-decrypt-file :as f]
            [clojure.java.io :as io]))
(deftest file-test
  (let [in (java.io.File/createTempFile "tc-in" ".txt")
        enc (java.io.File/createTempFile "tc-enc" ".txt")
        dec (java.io.File/createTempFile "tc-dec" ".txt")]
    (spit in "Harshil Darji")
    (is (= "Hlia rDsahrij" (f/encrypt-file (str in) (str enc) 6)))
    (is (= "Harshil Darji" (f/decrypt-file (str enc) (str dec) 6)))
    (is (thrown? Exception (f/encrypt-file "/nonexistent-xyz.txt" (str enc) 6)))))
