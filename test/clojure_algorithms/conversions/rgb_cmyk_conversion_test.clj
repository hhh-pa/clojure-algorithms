(ns clojure-algorithms.conversions.rgb-cmyk-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.rgb-cmyk-conversion :refer [rgb-to-cmyk]]))

(deftest test-rgb-to-cmyk
  (is (= [0 0 0 0] (rgb-to-cmyk 255 255 255)))
  (is (= [0 0 0 50] (rgb-to-cmyk 128 128 128)))
  (is (= [0 0 0 100] (rgb-to-cmyk 0 0 0)))
  (is (= [0 100 100 0] (rgb-to-cmyk 255 0 0)))
  (is (= [100 0 100 0] (rgb-to-cmyk 0 255 0)))
  (is (= [100 100 0 0] (rgb-to-cmyk 0 0 255)))
  (is (thrown? clojure.lang.ExceptionInfo (rgb-to-cmyk 255 200 "a")))
  (is (thrown? clojure.lang.ExceptionInfo (rgb-to-cmyk 255 255 999))))
