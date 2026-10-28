(ns clojure-algorithms.data-compression.coordinate-compression-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.coordinate-compression
             :refer [make-compressor compress decompress]]))

(deftest test-coordinate-compressor
  (let [cc (make-compressor [100 10 52 83])]
    (is (= 3 (compress cc 100)))
    (is (= 1 (compress cc 52)))
    (is (= 52 (decompress cc 1)))
    (is (= 2 (get (:coordinate-map cc) 83)))
    (is (= 83 (nth (:reverse-map cc) 2)))
    (is (= 10 (decompress cc 0)))
    (is (thrown? clojure.lang.ExceptionInfo (compress cc 80)))
    (is (thrown? clojure.lang.ExceptionInfo (compress cc 7)))
    (is (thrown? clojure.lang.ExceptionInfo (decompress cc 5))))
  (let [cc (make-compressor ["b" "a" "b"])]
    (is (= 1 (compress cc "b")))
    (is (= "a" (decompress cc 0)))))
