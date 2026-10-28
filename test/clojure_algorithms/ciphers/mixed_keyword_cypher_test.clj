(ns clojure-algorithms.ciphers.mixed-keyword-cypher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.mixed-keyword-cypher :as c]))
(deftest mixed-test
  (is (= "XKJGUFMJST" (c/mixed-keyword "college" "UNIVERSITY"))))
