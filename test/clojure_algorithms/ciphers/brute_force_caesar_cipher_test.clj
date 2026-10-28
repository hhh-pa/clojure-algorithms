(ns clojure-algorithms.ciphers.brute-force-caesar-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.brute-force-caesar-cipher :as c]))
(deftest brute-test
  (let [cands (c/decrypt-candidates "TMDETUX PMDVU")]
    (is (= 26 (count cands)))
    (is (= [12 "HARSHIL DARJI"] (nth cands 12)))
    (is (= [0 "TMDETUX PMDVU"] (nth cands 0)))))
