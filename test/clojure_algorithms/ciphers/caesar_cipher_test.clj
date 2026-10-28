(ns clojure-algorithms.ciphers.caesar-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.caesar-cipher :as c]))
(deftest caesar-test
  (is (= "bpm yCqks jzwEv nwF rCuxA wDmz Bpm tiHG lwo"
         (c/encrypt "The quick brown fox jumps over the lazy dog" 8)))
  (is (= "s nWjq dSjYW cWq" (c/encrypt "A very large key" 8000)))
  (is (= "f qtbjwhfxj fqumfgjy"
         (c/encrypt "a lowercase alphabet" 5 "abcdefghijklmnopqrstuvwxyz")))
  (is (= "The quick brown fox jumps over the lazy dog"
         (c/decrypt "bpm yCqks jzwEv nwF rCuxA wDmz Bpm tiHG lwo" 8)))
  (is (= "a lowercase alphabet"
         (c/decrypt "f qtbjwhfxj fqumfgjy" 5 "abcdefghijklmnopqrstuvwxyz")))
  (is (= "Please don't brute force me!"
         (get (c/brute-force "jFyuMy xIH'N vLONy zILwy Gy!") 20))))
