(ns clojure-algorithms.ciphers.gronsfeld-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.gronsfeld-cipher :as c]))
(deftest gronsfeld-test
  (is (= "LFNPP" (c/gronsfeld "hello" "412")))
  (is (= "IGOMQ" (c/gronsfeld "hello" "123")))
  (is (= "" (c/gronsfeld "" "123")))
  (is (= "YES, ¥€$ - _!@#%?" (c/gronsfeld "yes, ¥€$ - _!@#%?" "0")))
  (is (= "YFS, ¥€$ - _!@#%?" (c/gronsfeld "yes, ¥€$ - _!@#%?" "01")))
  (is (thrown? ArithmeticException (c/gronsfeld "hi" ""))))
