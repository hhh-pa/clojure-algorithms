(ns clojure-algorithms.ciphers.diffie-hellman-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.diffie-hellman :as d]))
(deftest dh-test
  (let [alice (d/make-dh) bob (d/make-dh)
        apub (d/generate-public-key alice)
        bpub (d/generate-public-key bob)]
    (is (= (d/generate-shared-key alice bpub)
           (d/generate-shared-key bob apub)))
    (is (= (d/generate-shared-key-static (d/get-private-key alice) bpub)
           (d/generate-shared-key-static (d/get-private-key bob) apub))))
  (is (thrown? IllegalArgumentException (d/make-dh 99))))
