(ns clojure-algorithms.ciphers.rabin-miller-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.rabin-miller :as r]))
(deftest prime-test
  (is (false? (r/is-prime-low-num 1)))
  (is (true? (r/is-prime-low-num 2)))
  (is (true? (r/is-prime-low-num 13)))
  (is (false? (r/is-prime-low-num 15)))
  (is (true? (r/is-prime-low-num 101)))
  (is (false? (r/is-prime-low-num 561)))
  (is (true? (r/rabin-miller 563)))
  (let [p (r/generate-large-prime 16)]
    (is (true? (r/is-prime-low-num p)))))
