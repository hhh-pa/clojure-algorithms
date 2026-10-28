(ns clojure-algorithms.ciphers.cryptomath-module-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.cryptomath-module :as c]))
(deftest mod-inverse-test
  (is (= 5 (c/find-mod-inverse 3 7)))
  (is (= 1 (mod (* 3 5) 7)))
  (is (= 7 (c/find-mod-inverse 3 10)))
  (is (= 3 (c/find-mod-inverse 4 11)))
  (is (= 15 (c/find-mod-inverse 7 26)))
  (is (= 1 (c/find-mod-inverse 1 5)))
  (is (thrown? IllegalArgumentException (c/find-mod-inverse 2 4)))
  (is (thrown? IllegalArgumentException (c/find-mod-inverse 6 9))))
