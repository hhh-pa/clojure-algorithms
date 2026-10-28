(ns clojure-algorithms.audio-filters.butterworth-filter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.audio-filters.butterworth-filter
             :refer [make-lowpass make-highpass make-bandpass make-allpass make-peak
                     make-lowshelf make-highshelf make-notch make-bandpass-peak]]))

(defn- approx
  [expected actual]
  (< (Math/abs (- (double expected) (double actual))) 1e-9))

(defn- approx-vecs [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map approx expected actual))))

(deftest factory-coefficients-test
  (testing "doctest coefficient values"
    (is (approx-vecs [1.0922959556412573 -1.9828897227476208 0.9077040443587427
                      0.004277569313094809 0.008555138626189618 0.004277569313094809]
                     (let [f (make-lowpass 1000 48000)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [1.0922959556412573 -1.9828897227476208 0.9077040443587427
                      0.9957224306869052 -1.9914448613738105 0.9957224306869052]
                     (let [f (make-highpass 1000 48000)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [1.0922959556412573 -1.9828897227476208 0.9077040443587427
                      0.06526309611002579 0 -0.06526309611002579]
                     (let [f (make-bandpass 1000 48000)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [1.0922959556412573 -1.9828897227476208 0.9077040443587427
                      0.9077040443587427 -1.9828897227476208 1.0922959556412573]
                     (let [f (make-allpass 1000 48000)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [1.0653405327119334 -1.9828897227476208 0.9346594672880666
                      1.1303715025601122 -1.9828897227476208 0.8696284974398878]
                     (let [f (make-peak 1000 48000 6)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [3.0409336710888786 -5.608870992220748 2.602157875636628
                      3.139954022810743 -5.591841778072785 2.5201667380627257]
                     (let [f (make-lowshelf 1000 48000 6)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [2.2229172136088806 -3.9587208137297303 1.7841414181566304
                      4.295432981120543 -7.922740859457287 3.6756456963725253]
                     (let [f (make-highshelf 1000 48000 6)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [1.0922959556412573 -1.9828897227476208 0.9077040443587427
                      1.0 -1.9828897227476208 1.0]
                     (let [f (make-notch 1000 48000)]
                       (into (:a-coeffs f) (:b-coeffs f)))))
    (is (approx-vecs [1.0922959556412573 -1.9828897227476208 0.9077040443587427
                      0.09229595564125725 0 -0.09229595564125725]
                     (let [f (make-bandpass-peak 1000 48000)]
                       (into (:a-coeffs f) (:b-coeffs f)))))))

(deftest validation-test
  (is (thrown? clojure.lang.ExceptionInfo (make-lowpass 0 48000)))
  (is (thrown? clojure.lang.ExceptionInfo (make-lowpass 24000 48000)))
  (is (thrown? clojure.lang.ExceptionInfo (make-lowpass 1000 0)))
  (is (thrown? clojure.lang.ExceptionInfo (make-lowpass 1000 48000 0)))
  (testing "notch skips validation like the Python reference"
    (is (= 3 (count (:a-coeffs (make-notch 1000 48000)))))))
