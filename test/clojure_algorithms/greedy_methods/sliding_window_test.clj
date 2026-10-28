(ns clojure-algorithms.greedy-methods.sliding-window-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.sliding-window :refer [sliding-window]]))

(deftest test-sliding-window
  (is (= 3 (sliding-window "abcabcbb")))
  (is (= 1 (sliding-window "bbbbb")))
  (is (= 3 (sliding-window "pwwkew")))
  (is (= 0 (sliding-window "")))
  (is (= 7 (sliding-window "abcdefg")))
  (is (= 3 (sliding-window "abccba")))
  (is (= 1 (sliding-window (apply str (repeat 1000 "a")))))
  (is (thrown? clojure.lang.ExceptionInfo (sliding-window 42))))
