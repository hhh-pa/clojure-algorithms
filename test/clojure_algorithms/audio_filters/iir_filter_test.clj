(ns clojure-algorithms.audio-filters.iir-filter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.audio-filters.iir-filter
             :refer [make-iir-filter set-coefficients process process-seq]]))

(deftest set-coefficients-test
  (testing "leading a0 may be omitted"
    (let [filt (set-coefficients (make-iir-filter 2) [-1.9 0.9] [1.0 -2.0 1.0])]
      (is (= [1.0 -1.9 0.9] (:a-coeffs filt)))))
  (testing "wrong coefficient counts raise"
    (is (thrown? clojure.lang.ExceptionInfo
                 (set-coefficients (make-iir-filter 2) [1.0 2.0 3.0 4.0] [1.0 2.0 3.0])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (set-coefficients (make-iir-filter 2) [1.0 2.0 3.0] [1.0 2.0])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (set-coefficients (make-iir-filter 2) [1.0] [1.0 2.0 3.0])))))

(deftest process-test
  (testing "silence in, silence out"
    (is (= [0.0 0.0] [(first (process (make-iir-filter 2) 0)) 0.0])))
  (testing "identity filter passes samples through"
    (let [[outs _] (process-seq (make-iir-filter 2) [0.5 -0.25 1.0])]
      (is (= [0.5 -0.25 1.0] outs))))
  (testing "state threads through successive calls"
    (let [filt (set-coefficients (make-iir-filter 1) [1.0 -0.5] [0.5 0.0])
          [y1 f1] (process filt 1.0)
          [y2 _] (process f1 1.0)]
      (is (< (Math/abs (- y1 0.5)) 1e-12))
      (is (< (Math/abs (- y2 0.75)) 1e-12)))))
