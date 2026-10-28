(ns clojure-algorithms.maths.pi-generator-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.pi-generator :as sut]))

(deftest test-fn-calculate-pi-examples
  (is (= Math/PI (Double/parseDouble (sut/calculate-pi 15))))
  (is (approx Math/PI (Double/parseDouble (sut/calculate-pi 50))))
  (is (approx Math/PI (Double/parseDouble (sut/calculate-pi 100))))
  (is (approx "3.14159265358979323846264338327950288419716939937510"
              (sut/calculate-pi 50)))
  (is (approx "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899"
              (sut/calculate-pi 80))))
