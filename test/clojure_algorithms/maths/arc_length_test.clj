(ns clojure-algorithms.maths.arc-length-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.arc-length :as sut]))

(deftest test-fn-arc-length-examples
  (is (approx 3.9269908169872414 (sut/arc-length 45 5)))
  (is (approx 31.415926535897928 (sut/arc-length 120 15)))
  (is (approx 15.707963267948966 (sut/arc-length 90 10))))
