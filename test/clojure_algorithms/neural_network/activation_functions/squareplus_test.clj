(ns clojure-algorithms.neural-network.activation-functions.squareplus-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.squareplus :refer [squareplus]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest squareplus-test
  (testing "doctest examples"
    (is (approx-vec= [2.5 1.06811457 0.22474487 0.12731349]
                     (squareplus [2.3 0.6 -2 -3.8] 2) 1e-6))
    (is (approx-vec= [0.0808119 0.72891979 1.11977651 0.15893419]
                     (squareplus [-9.2 -0.3 0.45 -4.56] 3) 1e-6)))
  (testing "large positive approximates identity"
    (is (approx= 100.0 (first (squareplus [100.0] 2)) 0.01))))
