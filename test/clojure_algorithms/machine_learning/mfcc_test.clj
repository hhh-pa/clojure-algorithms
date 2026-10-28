(ns clojure-algorithms.machine-learning.mfcc-test
  (:require [clojure-algorithms.machine-learning.mfcc :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(defn- round-to
  [x ndigits]
  (let [scale (Math/pow 10.0 (double ndigits))]
    (/ (Math/rint (* (double x) scale)) scale)))

(deftest test-fn-normalize-examples
  (let [out (sut/normalize [1 2 3 4 5])]
    (is (approx 1.0 (apply max out)))
    (is (approx 0.2 (apply min out))))
  (is (= :ValueError (:type (try (sut/normalize [])
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e)))))))

(deftest test-fn-audio-frames-examples
  (let [frames (sut/audio-frames (vec (apply concat (repeat 1000 [1 2 3 4 5 6 7
                                                                  8 9 10])))
                                 8000 10 512)]
    (is (= [126 512] [(count frames) (count (first frames))])))
  (testing "reflect padding shows at the edges"
    (let [frames (sut/audio-frames [1.0 2.0 3.0 4.0 5.0 6.0] 1000 1000 4)]
      ;; hop 1000ms @1kHz = 1000 samples; single frame of padded head
      (is (= 1 (count frames)))
      (is (approx [3.0 2.0 1.0 2.0] (first frames))))))

(deftest test-fn-calculate-fft-examples
  (let [fft (sut/calculate-fft [[1.0 2.0 3.0] [4.0 5.0 6.0]] 4)]
    (is (approx [[6.0 0.0] [-1.5 0.8660254] [-1.5 -0.8660254]]
                (first fft)))
    (is (= 2 (count fft))))
  (testing "single-tone energy concentrates in one bin"
    (let [fft (first (sut/calculate-fft
                      [(mapv #(Math/cos (* 2.0 Math/PI % (/ 8))) (range 8))]
                      8))]
      (is (< 3.9 (first (nth fft 1)) 4.1))
      (is (< (Math/abs (double (first (nth fft 2)))) 1e-6)))))

(deftest test-fn-calculate-signal-power-examples
  (is (approx [5 13 25 41]
              (sut/calculate-signal-power [[1 2] [2 3] [3 4] [4 5]])))
  (is (approx [[5.0] [25.0]]
              (sut/calculate-signal-power [[[1.0 2.0]] [[3.0 4.0]]]))))

(deftest test-fn-freq-mel-conversions
  (is (approx 999.99 (round-to (sut/freq-to-mel 1000) 2)))
  (is (approx 1000.01 (round-to (sut/mel-to-freq 999.99) 2)))
  (testing "round trip"
    (is (approx 1000.0 (sut/mel-to-freq (sut/freq-to-mel 1000))))))

(deftest test-fn-filter-points-examples
  (let [[points freqs] (sut/get-filter-points 8000 0 4000 4 512)]
    (is (= [0 20 51 95 161 256] points))
    (is (approx [0.0 324.46707094 799.33254207 1494.30973963 2511.42581671
                 4000.0]
                freqs))))

(deftest test-fn-get-filters-examples
  (let [filters (sut/get-filters [0 20 51 95 161 256] 512)]
    (is (= [4 257] [(count filters) (count (first filters))]))
    (testing "first filter ramps up then down"
      (is (approx 0.0 (nth (nth filters 0) 0)))
      (is (approx 1.0 (nth (nth filters 0) 20)))
      (is (approx 0.0 (nth (nth filters 0) 51))))))

(deftest test-fn-mel-spaced-filterbank-examples
  (is (approx 0.0004603981
              (round-to (nth (nth (sut/mel-spaced-filterbank 8000 10 1024) 0)
                             1)
                        10))))

(deftest test-fn-discrete-cosine-transform-examples
  (is (approx 0.44721
              (round-to (nth (nth (sut/discrete-cosine-transform 3 5) 0) 0)
                        5)))
  (testing "rows are orthonormal-ish"
    (let [dct (sut/discrete-cosine-transform 5 5)]
      (is (approx 1.0 (reduce + (map #(* % %) (nth dct 0))))))))

(deftest test-fn-mfcc-examples
  (testing "sine-wave MFCC matrix shape"
    (let [sample-rate 44100
          duration 2.0
          n (int (* sample-rate duration))
          audio (mapv #(* 0.5 (Math/sin (* 2.0 Math/PI 440.0 (/ % sample-rate))))
                      (range n))
          mfccs (sut/mfcc audio sample-rate)]
      (is (= [40 101] [(count mfccs) (count (first mfccs))]))))
  (is (= :ValueError (:type (try (sut/mfcc [] 44100)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e)))))))

(deftest test-fn-example-stub
  (is (= :UnsupportedOperation
         (:type (try (sut/example)
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
