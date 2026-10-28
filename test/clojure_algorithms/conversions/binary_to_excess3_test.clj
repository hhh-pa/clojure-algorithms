(ns clojure-algorithms.conversions.binary-to-excess3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.binary-to-excess3 :refer [binary-to-excess3]]))

(deftest test-binary-to-excess3
  (is (= "1101" (binary-to-excess3 "1010")))
  (is (= "0011" (binary-to-excess3 "0000")))
  (is (= "1100" (binary-to-excess3 "1001"))))
