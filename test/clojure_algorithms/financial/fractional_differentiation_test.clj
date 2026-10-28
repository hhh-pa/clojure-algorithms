(ns clojure-algorithms.financial.fractional-differentiation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.fractional-differentiation
             :refer [calculate-weights fracdiff-fixedwindow]]))

(deftest weights-test
  (is (= [1.0 -0.5 -0.125] (calculate-weights 0.5 3)))
  (is (= [1.0 -0.5 -0.125 -0.0625] (calculate-weights 0.5 4))))

(deftest fracdiff-test
  (let [result (fracdiff-fixedwindow [1 2 3 4 5 6 7 8 9 10] 0.5 3)]
    (is (= 10 (count result)))
    (is (every? #(Double/isNaN %) (take 3 result)))
    (is (= [2.25 2.625 3.0 3.375 3.75 4.125 4.5] (vec (drop 3 result)))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"window_size cannot exceed"
                        (fracdiff-fixedwindow [1 2] 0.5 3))))
