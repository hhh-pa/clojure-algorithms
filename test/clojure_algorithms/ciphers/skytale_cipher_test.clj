(ns clojure-algorithms.ciphers.skytale-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.skytale-cipher :as c]))
(deftest skytale-test
  (is (= "WA SVEFETNERDCEDL  C EIOR EAOE"
         (c/encrypt "WE ARE DISCOVERED FLEE AT ONCE" 3)))
  (is (= "WE ARE DISCOVERED FLEE AT ONCE"
         (c/decrypt "WA SVEFETNERDCEDL  C EIOR EAOE" 3)))
  (is (= "HELLO" (c/encrypt "HELLO" 1)))
  (is (= "HELLO" (c/decrypt "HELLO" 5)))
  (is (thrown? IllegalArgumentException (c/encrypt "HELLO" 0)))
  (is (thrown? IllegalArgumentException (c/decrypt "HELLO" -2))))
