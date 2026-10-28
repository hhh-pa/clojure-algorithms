(ns clojure-algorithms.financial.simple-moving-average-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.simple-moving-average
             :refer [simple-moving-average]]))

(defn- round2 [x] (when (some? x) (/ (Math/round (* 100.0 (double x))) 100.0)))

(deftest sma-examples
  (is (= [nil nil 12.33 13.33 14.0 14.33 16.0 17.0 18.0 19.0]
         (map round2 (simple-moving-average [10 12 15 13 14 16 18 17 19 21] 3))))
  (is (= [nil nil nil] (simple-moving-average [10 12 15] 5)))
  (is (= [10.0 12.0] (simple-moving-average [10 12] 1)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Window size must be a positive integer"
                        (simple-moving-average [10 12] 0))))
