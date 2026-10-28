(ns clojure-algorithms.ciphers.xor-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.xor-cipher :as c]))
(deftest xor-test
  (is (= [] (c/encrypt "" 5)))
  (is (= (vec "i`mmn!vdmu") (c/encrypt "hallo welt" 1)))
  (is (= "i`mmn!vdmu" (c/encrypt-string "hallo welt" 1)))
  (is (= "hallo welt" (c/encrypt-string "hallo welt" 256)))
  (is (= "hallo welt" (c/decrypt-string (c/encrypt-string "hallo welt" 67) 67))))
