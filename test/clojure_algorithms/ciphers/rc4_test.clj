(ns clojure-algorithms.ciphers.rc4-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.rc4 :as c]))
(deftest rc4-test
  (is (= [75 51 132 157 192] (take 5 (c/ksa (mapv int "Key")))))
  (is (= [235 159 119 129 183]
         (take 5 (c/prga (c/ksa (mapv int "Key"))))))
  (is (= (mapv int [0xbb 0xf3 0x16 0xe8 0xd9 0x40 0xaf 0x0a 0xd3])
         (c/encrypt (mapv int "Plaintext") (mapv int "Key"))))
  (is (= (mapv int "pedia")
         (c/decrypt (c/encrypt (mapv int "pedia") (mapv int "Wiki"))
                    (mapv int "Wiki"))))
  (is (thrown? IllegalArgumentException (c/encrypt [1 2] []))))
