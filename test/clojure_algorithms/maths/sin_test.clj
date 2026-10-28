(ns clojure-algorithms.maths.sin-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.sin :as sut]))

(deftest test-fn-sin-examples
  (is (approx 0.0 (sut/sin 0.0)))
  (is (approx 1.0 (sut/sin 90.0)))
  (is (approx 0.0 (sut/sin 180.0)))
  (is (approx -1.0 (sut/sin 270.0)))
  (is (approx 0.0118679603 (sut/sin 0.68)))
  (is (approx 0.0343762121 (sut/sin 1.97)))
  (is (approx 0.8987940463 (sut/sin 64.0)))
  (is (approx -0.9876883406 (sut/sin 9999.0)))
  (is (approx 0.5150380749 (sut/sin -689.0)))
  (is (approx 0.9999862922 (sut/sin 89.7))))
