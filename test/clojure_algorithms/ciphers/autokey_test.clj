(ns clojure-algorithms.ciphers.autokey-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.autokey :as c]))
(deftest autokey-test
  (is (= "jsqqs avvwo" (c/encrypt "hello world" "coffee")))
  (is (= "vvjfpk wj ohvp su ddylsv"
         (c/encrypt "coffee is good as python" "TheAlgorithms")))
  (is (= "hello world" (c/decrypt "jsqqs avvwo" "coffee")))
  (is (= "coffee is good as python"
         (c/decrypt "vvjfpk wj ohvp su ddylsv" "TheAlgorithms")))
  (is (thrown? IllegalArgumentException (c/encrypt "" "k")))
  (is (thrown? IllegalArgumentException (c/encrypt "hi" "")))
  (is (thrown? IllegalArgumentException (c/decrypt "" "k"))))
