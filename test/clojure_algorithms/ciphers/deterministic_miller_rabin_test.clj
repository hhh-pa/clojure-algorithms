(ns clojure-algorithms.ciphers.deterministic-miller-rabin-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.deterministic-miller-rabin :as m]))
(deftest miller-rabin-test
  (is (true? (m/miller-rabin 2)))
  (is (false? (m/miller-rabin 1)))
  (is (false? (m/miller-rabin 561)))
  (is (true? (m/miller-rabin 563)))
  (is (false? (m/miller-rabin 838201)))
  (is (true? (m/miller-rabin 838207)))
  (is (false? (m/miller-rabin 17316001)))
  (is (true? (m/miller-rabin 17316017)))
  (is (false? (m/miller-rabin 3078386641)))
  (is (true? (m/miller-rabin 3078386653)))
  (is (false? (m/miller-rabin 1713045574801)))
  (is (true? (m/miller-rabin 1713045574819))))
