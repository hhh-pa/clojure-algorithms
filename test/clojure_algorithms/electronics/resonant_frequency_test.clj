(ns clojure-algorithms.electronics.resonant-frequency-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.resonant-frequency :as rf]))

(deftest resonant-frequency-test
  (testing "resonant frequency value"
    (let [[n v] (rf/resonant-frequency 10 5)]
      (is (= "Resonant frequency" n))
      (is (< (Math/abs (- v 0.022507907903927652)) 1e-15))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (rf/resonant-frequency 0 5)))
    (is (thrown? IllegalArgumentException (rf/resonant-frequency 10 0)))))
