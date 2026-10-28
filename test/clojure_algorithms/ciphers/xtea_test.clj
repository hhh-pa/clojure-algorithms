(ns clojure-algorithms.ciphers.xtea-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.xtea :as c]))
(deftest xtea-test
  (let [zeros8 (vec (repeat 8 0)) zeros16 (vec (repeat 16 0))]
    (is (= "fc924d124ad0ed50"
           (apply str (map #(format "%02x" %) (c/xtea-encrypt zeros8 zeros16))))))
  (let [msg (mapv int "hello!!!") k (mapv int "sixteenbyteskey!")]
    (is (= msg (c/xtea-decrypt (c/xtea-encrypt msg k) k))))
  (is (thrown? IllegalArgumentException (c/xtea-encrypt [1 2 3] (vec (repeat 16 0)))))
  (is (thrown? IllegalArgumentException (c/xtea-decrypt (vec (repeat 8 0)) [1]))))
