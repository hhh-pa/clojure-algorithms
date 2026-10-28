(ns clojure-algorithms.ciphers.atbash-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.atbash :as c]))
(deftest atbash-test
  (is (= "ZYXWVUT" (c/atbash "ABCDEFG")))
  (is (= "zD;;123YC" (c/atbash "aW;;123BX")))
  (is (= "ZYXWVUT" (c/atbash-slow "ABCDEFG")))
  (is (= "zD;;123YC" (c/atbash-slow "aW;;123BX")))
  (is (= "test" (c/atbash (c/atbash "test")))))
