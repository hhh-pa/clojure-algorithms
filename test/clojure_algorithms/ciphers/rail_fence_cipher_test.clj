(ns clojure-algorithms.ciphers.rail-fence-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.rail-fence-cipher :as c]))
(deftest rail-test
  (is (= "HWe olordll" (c/encrypt "Hello World" 4)))
  (is (= "Hello World" (c/decrypt "HWe olordll" 4)))
  (is (= "Hello World" (c/decrypt "Hello World" 100)))
  (is (= "Hello World" (get (c/bruteforce "HWe olordll") 4)))
  (is (thrown? IllegalArgumentException (c/encrypt "hi" 0)))
  (is (thrown? IllegalArgumentException (c/decrypt "hi" -10))))
