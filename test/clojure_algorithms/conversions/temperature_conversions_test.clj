(ns clojure-algorithms.conversions.temperature-conversions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.temperature-conversions
             :refer [celsius-to-fahrenheit celsius-to-kelvin celsius-to-rankine
                     fahrenheit-to-celsius fahrenheit-to-kelvin fahrenheit-to-rankine
                     kelvin-to-celsius kelvin-to-fahrenheit kelvin-to-rankine
                     rankine-to-celsius rankine-to-fahrenheit rankine-to-kelvin
                     reaumur-to-kelvin reaumur-to-fahrenheit reaumur-to-celsius
                     reaumur-to-rankine]]))

(deftest test-temperature-conversions
  (is (= 212.0 (celsius-to-fahrenheit 100)))
  (is (= 32.0 (celsius-to-fahrenheit 0)))
  (is (= 273.15 (celsius-to-kelvin 0)))
  (is (= 373.15 (celsius-to-kelvin 100)))
  (is (= 491.67 (celsius-to-rankine 0)))
  (is (= 0.0 (fahrenheit-to-celsius 32)))
  (is (= 100.0 (fahrenheit-to-celsius 212)))
  (is (= 273.15 (fahrenheit-to-kelvin 32)))
  (is (= 491.67 (fahrenheit-to-rankine 32)))
  (is (= 0.0 (kelvin-to-celsius 273.15)))
  (is (= 32.0 (kelvin-to-fahrenheit 273.15)))
  (is (= 491.67 (kelvin-to-rankine 273.15)))
  (is (= 0.0 (rankine-to-celsius 491.67)))
  (is (= 32.0 (rankine-to-fahrenheit 491.67)))
  (is (= 273.15 (rankine-to-kelvin 491.67)))
  (is (= 273.15 (reaumur-to-kelvin 0)))
  (is (= 32.0 (reaumur-to-fahrenheit 0)))
  (is (= 0.0 (reaumur-to-celsius 0)))
  (is (= 491.67 (reaumur-to-rankine 0)))
  (is (= 536.67 (reaumur-to-rankine 20.0)))
  (is (= 581.67 (reaumur-to-rankine 40)))
  (is (= 212.0 (celsius-to-fahrenheit 100 2)))
  (is (thrown? clojure.lang.ExceptionInfo (reaumur-to-rankine "reaumur"))))
