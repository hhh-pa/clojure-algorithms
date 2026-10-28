(ns clojure-algorithms.hashes.crc32-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.crc32 :refer [crc32 crc32-str crc32-table]]))

(deftest test-table
  (is (= 256 (count crc32-table)))
  (is (= 0 (nth crc32-table 0)))
  (is (= 0xEDB88320 (nth crc32-table 128))))

(deftest test-crc32
  (is (= 1243066710 (crc32-str "Hello World")))
  (is (= 0 (crc32 (byte-array 0))))
  (is (= 1095738169 (crc32-str "The quick brown fox jumps over the lazy dog")))
  (is (= 3904355907 (crc32-str "a")))
  (is (= 891568578 (crc32-str "abc")))
  (is (= 3421780262 (crc32-str "123456789")))
  (is (= 2742599054 (crc32-str "Python")))
  (is (= 3866870335 (crc32-str "Algorithms")))
  (is (= 4128576900 (crc32-str "CRC32"))))
