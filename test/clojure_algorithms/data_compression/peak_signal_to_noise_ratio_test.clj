(ns clojure-algorithms.data-compression.peak-signal-to-noise-ratio-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.peak-signal-to-noise-ratio
             :refer [peak-signal-to-noise-ratio]]))

(deftest test-psnr
  (is (= 100 (peak-signal-to-noise-ratio [0 0] [0 0])))
  (is (= 100 (peak-signal-to-noise-ratio [[1 2] [3 4]] [[1 2] [3 4]])))
  (let [v (peak-signal-to-noise-ratio [0 0] [1 1])]
    (is (< (Math/abs (- v 48.13080360867911)) 1e-9)))
  (let [v (peak-signal-to-noise-ratio [255] [0])]
    (is (< (Math/abs (- v 0.0)) 1e-9))))
