(ns clojure-algorithms.strings.z-function-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.z-function :refer [z-function find-pattern]]))

(deftest test-z-function
  (is (= [0 0 0 1 0 1 0 4 0 0 1] (z-function "abracadabra")))
  (is (= [0 3 2 1] (z-function "aaaa")))
  (is (= [0 0 0 4 0 0 1] (z-function "zxxzxxz"))))

(deftest test-find-pattern
  (is (= 2 (find-pattern "abr" "abracadabra")))
  (is (= 4 (find-pattern "a" "aaaa")))
  (is (= 2 (find-pattern "xz" "zxxzxxz")))
  (is (= 0 (find-pattern "aa" "a"))))
