(ns clojure-algorithms.hashes.adler32-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.adler32 :refer [adler32]]))

(deftest test-adler32
  (is (= 363791387 (adler32 "Algorithms")))
  (is (= 708642122 (adler32 "go adler em all")))
  (is (= 1 (adler32 ""))))
