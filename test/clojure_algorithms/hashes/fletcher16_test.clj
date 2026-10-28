(ns clojure-algorithms.hashes.fletcher16-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.fletcher16 :refer [fletcher16]]))

(deftest test-fletcher16
  (is (= 6752 (fletcher16 "hello world")))
  (is (= 28347 (fletcher16 "onethousandfourhundredthirtyfour")))
  (is (= 5655 (fletcher16 "The quick brown fox jumps over the lazy dog.")))
  (is (= 0 (fletcher16 ""))))
