(ns clojure-algorithms.hashes.luhn-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.luhn :refer [is-luhn]]))

(deftest test-is-luhn
  (is (= [false false false true false false false false false false]
         (mapv (comp is-luhn str)
               [79927398710 79927398711 79927398712 79927398713 79927398714
                79927398715 79927398716 79927398717 79927398718 79927398719])))
  (is (true? (is-luhn "79927398713")))
  (is (false? (is-luhn "79927398714"))))
