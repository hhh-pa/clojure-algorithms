(ns clojure-algorithms.maths.pi-generator-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.pi-generator :as sut]))

(deftest test-fn-calculate-pi-examples
  (is (approx "3.14159265358979323846264338327950288419716939937510" (sut/calculate-pi 50)))
  (is (approx "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899" (sut/calculate-pi 80))))
