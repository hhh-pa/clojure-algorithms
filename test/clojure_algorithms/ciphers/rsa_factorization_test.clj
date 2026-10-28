(ns clojure-algorithms.ciphers.rsa-factorization-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.rsa-factorization :as c]))
(deftest factor-test
  (is (= [149 173] (c/rsafactor 3 16971 25777)))
  (is (= [113 241] (c/rsafactor 7331 11 27233)))
  (is (= [89 199] (c/rsafactor 4021 13 17711))))
