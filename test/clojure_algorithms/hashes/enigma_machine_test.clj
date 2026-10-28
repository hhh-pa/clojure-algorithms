(ns clojure-algorithms.hashes.enigma-machine-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.enigma-machine :refer [encode-or-decode]]))

(deftest test-encode-or-decode
  (is (= [[\/ \0 \' \% \space] 3] (encode-or-decode "hello" 3)))
  (let [[code token] (encode-or-decode "abc" 0)]
    (is (= 3 (count code)))
    (is (= 0 token)))
  (is (= (encode-or-decode "hello" 3) (encode-or-decode "hello" 3))))
