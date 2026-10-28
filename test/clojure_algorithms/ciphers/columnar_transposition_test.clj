(ns clojure-algorithms.ciphers.columnar-transposition-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.columnar-transposition :as c]))
(deftest columnar-test
  (let [pt "WE ARE DISCOVERED. FLEE AT ONCE"
        ct (c/encrypt pt "ZEBRAS")]
    (is (= pt (c/decrypt ct "ZEBRAS"))))
  (is (= "HELLO" (c/encrypt "HELLO" "A")))
  (is (= "HELLO" (c/decrypt "HELLO" "A")))
  (is (= "EHLLO" (c/encrypt "HELLO" "HELLO")))
  (is (= "HELLO" (c/decrypt "EHLLO" "HELLO")))
  (is (thrown? IllegalArgumentException (c/encrypt "HELLO" ""))))
