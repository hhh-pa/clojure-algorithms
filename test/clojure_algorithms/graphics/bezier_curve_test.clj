(ns clojure-algorithms.graphics.bezier-curve-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphics.bezier-curve
             :refer [make-curve basis-function bezier-curve-function
                     derivative]]))

(deftest test-bezier-curve
  (let [curve (make-curve [[1 1] [1 2]])]
    (is (= [1.0 0.0] (mapv double (basis-function curve 0))))
    (is (= [0.0 1.0] (mapv double (basis-function curve 1))))
    (is (= [1.0 1.0] (mapv double (bezier-curve-function curve 0))))
    (is (= [1.0 2.0] (mapv double (bezier-curve-function curve 1))))
    (is (= [0.0 1.0] (mapv double (derivative curve 0.5)))))
  (is (thrown? IllegalArgumentException
               (bezier-curve-function (make-curve [[0 0] [1 1]]) 2))))
