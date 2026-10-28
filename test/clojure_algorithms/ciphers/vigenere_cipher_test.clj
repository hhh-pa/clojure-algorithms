(ns clojure-algorithms.ciphers.vigenere-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.vigenere-cipher :as c]))
(deftest vigenere-test
  (is (= "Akij ra Odrjqqs Gaisq muod Mphumrs."
         (c/encrypt-message "HDarji" "This is Harshil Darji from Dharmaj.")))
  (is (= "This is Harshil Darji from Dharmaj."
         (c/decrypt-message "HDarji" "Akij ra Odrjqqs Gaisq muod Mphumrs."))))
