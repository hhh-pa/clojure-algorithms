(ns clojure-algorithms.conversions.rgb-hsv-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.rgb-hsv-conversion
             :refer [hsv-to-rgb rgb-to-hsv approximately-equal-hsv]]))

(deftest test-hsv-to-rgb
  (is (= [0 0 0] (hsv-to-rgb 0 0 0)))
  (is (= [255 255 255] (hsv-to-rgb 0 0 1)))
  (is (= [255 0 0] (hsv-to-rgb 0 1 1)))
  (is (= [255 255 0] (hsv-to-rgb 60 1 1)))
  (is (= [0 255 0] (hsv-to-rgb 120 1 1)))
  (is (= [0 0 255] (hsv-to-rgb 240 1 1)))
  (is (= [255 0 255] (hsv-to-rgb 300 1 1)))
  (is (= [64 128 128] (hsv-to-rgb 180 0.5 0.5)))
  (is (= [193 196 224] (hsv-to-rgb 234 0.14 0.88)))
  (is (= [128 32 80] (hsv-to-rgb 330 0.75 0.5)))
  (is (thrown? clojure.lang.ExceptionInfo (hsv-to-rgb 400 0 0))))

(deftest test-rgb-to-hsv
  (is (approximately-equal-hsv (rgb-to-hsv 0 0 0) [0 0 0]))
  (is (approximately-equal-hsv (rgb-to-hsv 255 255 255) [0 0 1]))
  (is (approximately-equal-hsv (rgb-to-hsv 255 0 0) [0 1 1]))
  (is (approximately-equal-hsv (rgb-to-hsv 255 255 0) [60 1 1]))
  (is (approximately-equal-hsv (rgb-to-hsv 0 255 0) [120 1 1]))
  (is (approximately-equal-hsv (rgb-to-hsv 0 0 255) [240 1 1]))
  (is (approximately-equal-hsv (rgb-to-hsv 255 0 255) [300 1 1]))
  (is (approximately-equal-hsv (rgb-to-hsv 64 128 128) [180 0.5 0.5]))
  (is (approximately-equal-hsv (rgb-to-hsv 193 196 224) [234 0.14 0.88]))
  (is (approximately-equal-hsv (rgb-to-hsv 128 32 80) [330 0.75 0.5]))
  (is (thrown? clojure.lang.ExceptionInfo (rgb-to-hsv 300 0 0))))

(deftest test-approximately-equal-hsv
  (is (approximately-equal-hsv [0 0 0] [0 0 0]))
  (is (approximately-equal-hsv [180 0.5 0.3] [179.9999 0.500001 0.30001]))
  (is (not (approximately-equal-hsv [0 0 0] [1 0 0])))
  (is (not (approximately-equal-hsv [180 0.5 0.3] [179.9999 0.6 0.30001]))))
