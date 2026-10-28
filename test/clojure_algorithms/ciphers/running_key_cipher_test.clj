(ns clojure-algorithms.ciphers.running-key-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.running-key-cipher :as c]))
(deftest running-key-test
  (let [key "How does the duck know that? said Victor"
        ct (c/running-key-encrypt key "DEFEND THIS")]
    (is (= "DEFENDTHIS" (c/running-key-decrypt key ct)))))
