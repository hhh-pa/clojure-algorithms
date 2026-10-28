(ns clojure-algorithms.hashes.elf-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.elf :refer [elf-hash]]))

(deftest test-elf-hash
  (is (= 253956621 (elf-hash "lorem ipsum")))
  (is (= 0 (elf-hash ""))))
