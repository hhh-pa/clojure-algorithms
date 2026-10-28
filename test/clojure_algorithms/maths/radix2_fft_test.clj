(ns clojure-algorithms.maths.radix2-fft-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.radix2-fft :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-FFT))
  (is (some? #'sut/FFT-dft))
  (is (some? #'sut/FFT-multiply))
  (is (some? #'sut/FFT-str)))
