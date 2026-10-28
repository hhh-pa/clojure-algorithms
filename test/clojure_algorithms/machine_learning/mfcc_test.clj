(ns clojure-algorithms.machine-learning.mfcc-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.mfcc :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/mfcc))
  (is (some? #'sut/normalize))
  (is (some? #'sut/audio-frames))
  (is (some? #'sut/calculate-fft))
  (is (some? #'sut/calculate-signal-power))
  (is (some? #'sut/freq-to-mel))
  (is (some? #'sut/mel-to-freq))
  (is (some? #'sut/mel-spaced-filterbank))
  (is (some? #'sut/get-filters))
  (is (some? #'sut/get-filter-points))
  (is (some? #'sut/discrete-cosine-transform))
  (is (some? #'sut/example)))
