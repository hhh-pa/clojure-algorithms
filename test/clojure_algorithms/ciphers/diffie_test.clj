(ns clojure-algorithms.ciphers.diffie-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.diffie :as d]))
(deftest primitive-test
  (is (= 3 (d/find-primitive 7)))
  (is (= 2 (d/find-primitive 11)))
  (is (nil? (d/find-primitive 8))))
