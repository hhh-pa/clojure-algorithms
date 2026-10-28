(ns clojure-algorithms.maths.numerical-analysis.nth-root-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.nth-root :as sut]))

(defn- py-round
  [v n]
  (Double/parseDouble
   (.toPlainString (.setScale (BigDecimal. ^double (double v)) n
                              java.math.RoundingMode/HALF_EVEN))))

(deftest test-fn-nth-root-examples
  (is (= 3.0 (py-round (sut/nth-root 9 2) 1)))
  (is (= -2 (Math/round (double (sut/nth-root -8 3 0.001)))))
  (is (= 4 (Math/round (double (sut/nth-root 256 4 0.001)))))
  (is (= 1.41421 (py-round (sut/nth-root 2 2) 5)))
  (is (= 0.5 (py-round (sut/nth-root 0.25 2 0.00000001) 1)))
  (is (= -0.66667 (py-round (sut/nth-root (/ -8.0 27) 3 0.0000001) 5)))
  (is (approx 0.0 (sut/nth-root 0 2 0.1)))
  (is (approx 0.0 (sut/nth-root 0.0 5)))
  (is (every? (fn [k]
                (<= (Math/abs (- (double (sut/nth-root k k 0.00000001))
                                 (Math/pow (double k) (/ 1.0 k))))
                    1e-10))
              (range 1 10)))
  (is (thrown? Exception (sut/nth-root "invalid input" 3 0.0001)))
  (is (thrown? Exception (sut/nth-root 4 0.5 0.0001)))
  (is (thrown? Exception (sut/nth-root 16 -4 0.001)))
  (is (thrown? Exception (sut/nth-root 4 2 "0.000001")))
  (is (thrown? Exception (sut/nth-root 9 2 -0.01)))
  (is (thrown? Exception (sut/nth-root -256 4 0.0001))))
