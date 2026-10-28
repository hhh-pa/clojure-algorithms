(ns clojure-algorithms.data-compression.move-to-front-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.move-to-front
             :refer [move-to-front-encode move-to-front-decode]]))

(deftest test-encode
  (is (= [1 1 13 1 1 1]
         (move-to-front-encode "banana" "abcdefghijklmnopqrstuvwxyz")))
  (is (= [1 1 2 1 1 1] (move-to-front-encode "banana" "abn")))
  (is (= [] (move-to-front-encode "" "abc")))
  (is (thrown? clojure.lang.ExceptionInfo
               (move-to-front-encode "bad" "abc")))
  (is (thrown? clojure.lang.ExceptionInfo
               (move-to-front-encode "abc" "aba"))))

(deftest test-decode
  (is (= "banana" (move-to-front-decode [1 1 13 1 1 1]
                                        "abcdefghijklmnopqrstuvwxyz")))
  (is (= "banana" (move-to-front-decode [1 1 2 1 1 1] "abn")))
  (is (= "" (move-to-front-decode [] "abc")))
  (is (thrown? clojure.lang.ExceptionInfo (move-to-front-decode [3] "abc")))
  (is (thrown? clojure.lang.ExceptionInfo (move-to-front-decode [-1] "abc"))))

(deftest test-roundtrip
  (is (= "banana" (move-to-front-decode
                   (move-to-front-encode "banana" "abcdefghijklmnopqrstuvwxyz")
                   "abcdefghijklmnopqrstuvwxyz"))))
