(ns clojure-algorithms.ciphers.shuffled-shift-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.shuffled-shift-cipher :as c]))
(deftest shuffled-test
  (let [ssc (c/make-cipher "4PYIXyqeQZr44")]
    (is (= "d>**-1z6&'5z'5z:z+-='$'>=zp:>5:#z<'.&>#"
           (c/encrypt ssc "Hello, this is a modified Caesar cipher")))
    (is (= "Hello, this is a modified Caesar cipher"
           (c/decrypt ssc "d>**-1z6&'5z'5z:z+-='$'>=zp:>5:#z<'.&>#"))))
  (let [ssc (c/make-cipher)]
    (is (= "Hi there!" (c/decrypt ssc (c/encrypt ssc "Hi there!"))))))
