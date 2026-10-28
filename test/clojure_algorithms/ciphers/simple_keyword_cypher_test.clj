(ns clojure-algorithms.ciphers.simple-keyword-cypher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.simple-keyword-cypher :as c]))
(deftest keyword-test
  (is (= "Helo Wrd" (c/remove-duplicates "Hello World!!")))
  (is (= "CYJJM VMQJB!!"
         (c/encipher "Hello World!!" (c/create-cipher-map "Goodbye!!"))))
  (let [m (c/create-cipher-map "Goodbye!!")]
    (is (= "HELLO WORLD!!" (c/decipher (c/encipher "Hello World!!" m) m)))))
