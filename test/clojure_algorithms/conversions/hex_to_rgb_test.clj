(ns clojure-algorithms.conversions.hex-to-rgb-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.hex-to-rgb :refer [hex-to-rgb]]))

(deftest test-hex-to-rgb
  (is (= "rgb(255, 0, 0)" (hex-to-rgb "#FF0000")))
  (is (= "rgb(0, 255, 0)" (hex-to-rgb "#00FF00")))
  (is (= "rgb(17, 34, 51)" (hex-to-rgb "#123")))
  (is (= "rgb(0, 0, 0)" (hex-to-rgb "#000000")))
  (is (= "rgb(255, 255, 255)" (hex-to-rgb "#FFFFFF")))
  (is (= "rgb(0, 136, 255)" (hex-to-rgb "#0088FF")))
  (is (thrown? clojure.lang.ExceptionInfo (hex-to-rgb "#0032ccAA")))
  (is (thrown? clojure.lang.ExceptionInfo (hex-to-rgb "#0032"))))
