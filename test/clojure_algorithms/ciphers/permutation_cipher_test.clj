(ns clojure-algorithms.ciphers.permutation-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.permutation-cipher :as c]))
(deftest permutation-test
  (let [[enc key] (c/encrypt "HELLO WORLD")]
    (is (= "HELLO WORLD" (c/decrypt enc key))))
  (let [[enc key] (c/encrypt "ABCD" [2 0 1 3] 4)]
    (is (= "CABD" enc))
    (is (= "ABCD" (c/decrypt enc key)))))
