(ns clojure-algorithms.hashes.hamming-code-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.hamming-code
             :refer [emitter-converter receptor-converter text-from-bits text-to-bits]]))

(deftest test-text-bits
  (is (= "011011010111001101100111" (text-to-bits "msg")))
  (is (= "msg" (text-from-bits "011011010111001101100111"))))

(deftest test-emitter
  (is (= ["1" "1" "1" "1" "0" "1" "0" "0" "1" "0" "1" "1" "1" "1" "1" "1"]
         (emitter-converter 4 "101010111111")))
  (is (thrown? clojure.lang.ExceptionInfo (emitter-converter 5 "101010111111"))))

(deftest test-receptor
  (is (= [["1" "0" "1" "0" "1" "0" "1" "1" "1" "1" "1" "1"] true]
         (receptor-converter 4 "1111010010111111"))))
