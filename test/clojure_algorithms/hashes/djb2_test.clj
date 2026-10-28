(ns clojure-algorithms.hashes.djb2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.djb2 :refer [djb2]]))

(deftest test-djb2
  (is (= 3782405311 (djb2 "Algorithms")))
  (is (= 1609059040 (djb2 "scramble bits")))
  (is (= 5381 (djb2 ""))))
