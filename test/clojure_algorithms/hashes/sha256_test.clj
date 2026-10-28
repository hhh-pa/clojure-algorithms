(ns clojure-algorithms.hashes.sha256-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.sha256 :refer [sha256 sha256-bytes]]))

(deftest test-sha256
  (is (= "18885f27b5af9012df19e496460f9294d5ab76128824c6f993787004f6d9a7db" (sha256 "Python")))
  (is (= "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9" (sha256 "hello world")))
  (is (= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (sha256 "")))
  (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (sha256 "abc")))
  (is (= (sha256 "Python") (sha256-bytes (.getBytes "Python" "UTF-8")))))
