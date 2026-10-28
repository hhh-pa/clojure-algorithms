(ns clojure-algorithms.ciphers.decrypt-caesar-with-chi-squared-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.decrypt-caesar-with-chi-squared :as c]))
(deftest chi-test
  (is (= [7 3129.228005747531
          "why is the caesar cipher so popular? it is too easy to crack!"]
         (c/decrypt-caesar-with-chi-squared
          "dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!")))
  (is (= [10 233.35343938980898 "short string"]
         (c/decrypt-caesar-with-chi-squared "crybd cdbsxq")))
  (is (= "Short String"
         (nth (c/decrypt-caesar-with-chi-squared "Crybd Cdbsxq" :case-sensitive true) 2))))
