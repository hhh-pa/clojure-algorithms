(ns clojure-algorithms.linear-algebra.src.gaussian-elimination-pivoting-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.gaussian-elimination-pivoting
             :refer [solve-linear-system]]))

(defn- approx-vec [a b]
  (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9) a b)))

(deftest test-solve-linear-system
  (is (approx-vec [2.0 3.0 -1.0]
                  (solve-linear-system [[2 1 -1 8] [-3 -1 2 -11] [-2 1 2 -3]])))
  (is (approx-vec [-12.823529411764705 -2.294117647058823 11.764705882352937 19.235294117647054]
                  (solve-linear-system [[5.0 -5.0 -3.0 4.0 -11.0]
                                        [1.0 -4.0 6.0 -4.0 -10.0]
                                        [-2.0 -5.0 4.0 -5.0 -12.0]
                                        [-3.0 -3.0 5.0 -5.0 8.0]])))
  (is (thrown? clojure.lang.ExceptionInfo (solve-linear-system [[0 0 0]])))
  (is (thrown? clojure.lang.ExceptionInfo (solve-linear-system [[0 0 0] [0 0 0]]))))
