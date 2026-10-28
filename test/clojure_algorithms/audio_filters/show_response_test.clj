(ns clojure-algorithms.audio-filters.show-response-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.audio-filters.show-response
             :refer [get-bounds impulse-response show-frequency-response
                     show-phase-response]]
            [clojure-algorithms.audio-filters.iir-filter
             :refer [make-iir-filter process]]))

(deftest get-bounds-test
  (testing "doctest equivalent: linspace(-20, 20, 1000)"
    (let [array (mapv #(- (* 40 (/ % 999)) 20) (range 1000))]
      (is (= [-20 20] (get-bounds array 1000)))))
  (testing "range widens beyond the data"
    (is (= [-20 20] (get-bounds (vec (repeat 100 0.0)) 100))))
  (testing "range follows outlying data"
    (let [data (assoc (vec (repeat 100 0.0)) 40 45.0)]
      (is (= [-20 45.0] (get-bounds data 100))))))

(deftest impulse-response-test
  (testing "identity filter reproduces the unit impulse"
    (let [resp (impulse-response (fn [f sample]
                                   (let [f (or f (make-iir-filter 2))]
                                     (process f sample))))]
      (is (= 512 (count resp)))
      (is (= 1.0 (first resp)))
      (is (every? zero? (rest resp))))))

(deftest plotting-stub-test
  (testing "matplotlib-dependent entry points throw with explanation"
    (is (thrown? UnsupportedOperationException (show-frequency-response nil 48000)))
    (is (thrown? UnsupportedOperationException (show-phase-response nil 48000)))))
