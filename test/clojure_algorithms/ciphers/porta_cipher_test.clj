(ns clojure-algorithms.ciphers.porta-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.porta-cipher :as c]))
(deftest porta-test
  (is (= [["ABCDEFGHIJKLM" "UVWXYZNOPQRST"] ["ABCDEFGHIJKLM" "NOPQRSTUVWXYZ"]
          ["ABCDEFGHIJKLM" "STUVWXYZNOPQR"] ["ABCDEFGHIJKLM" "QRSTUVWXYZNOP"]
          ["ABCDEFGHIJKLM" "WXYZNOPQRSTUV"] ["ABCDEFGHIJKLM" "UVWXYZNOPQRST"]]
         (c/generate-table "marvin")))
  (is (= "QRACRWU" (c/encrypt "marvin" "jessica")))
  (is (= "JESSICA" (c/decrypt "marvin" "QRACRWU")))
  (is (= [0 12] (c/get-position (first (c/generate-table "marvin")) \M)))
  (is (= \T (c/get-opponent (first (c/generate-table "marvin")) \M))))
