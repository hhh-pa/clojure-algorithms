(ns clojure-algorithms.maths.radix2-fft-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.radix2-fft :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-radix2-fft-unsupported
  (is (thrown? Exception (sut/make-FFT [0 1 0 2] [2 3 4 0])))
  (is (thrown? Exception (sut/FFT-dft nil "A")))
  (is (thrown? Exception (sut/FFT-multiply nil)))
  (is (thrown? Exception (sut/FFT-str nil)))
  (is (= :UnsupportedOperation (err-type #(sut/make-FFT [0 1 0 2] [2 3 4 0]))))
  (is (= :UnsupportedOperation (err-type #(sut/FFT-dft nil "A"))))
  (is (= :UnsupportedOperation (err-type #(sut/FFT-multiply nil))))
  (is (= :UnsupportedOperation (err-type #(sut/FFT-str nil)))))
