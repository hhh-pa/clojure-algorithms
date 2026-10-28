(ns clojure-algorithms.other.quine-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.quine :as q]
            [clojure.string :as str]))

;; Placeholder test: a file-level byte-exact quine cannot be ported
;; verbatim, so we verify the self-reproduction property instead.
(deftest quine-test
  (testing "quine form reproduces itself"
    (is (true? (q/self-reproducing?)))
    (is (str/includes? (q/quine-form) "quine-template"))))
