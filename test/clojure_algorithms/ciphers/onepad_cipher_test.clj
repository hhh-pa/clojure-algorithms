(ns clojure-algorithms.ciphers.onepad-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.onepad-cipher :as c]))
(deftest onepad-test
  (is (= [[] []] (c/encrypt "")))
  (is (= "" (c/decrypt [] [])))
  (is (= "" (c/decrypt [35] [])))
  (let [[cipher key] (c/encrypt "Hello" (java.util.Random. 1))]
    (is (= 5 (count cipher) (count key)))
    (is (= "Hello" (c/decrypt cipher key)))))
