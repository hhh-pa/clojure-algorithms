(ns clojure-algorithms.hashes.chaos-machine-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.chaos-machine :refer [pull! push! reset-machine!]]))

(deftest test-chaos-machine
  (reset-machine!)
  (is (integer? (pull!)))
  (is (<= 0 (pull!) 0xFFFFFFFF))
  (reset-machine!)
  (let [a (pull!)]
    (reset-machine!)
    (is (= a (pull!))))
  (reset-machine!)
  (push! 12345)
  (is (<= 0 (pull!) 0xFFFFFFFF)))
