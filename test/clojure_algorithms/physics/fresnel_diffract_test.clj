(ns clojure-algorithms.physics.fresnel-diffract-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.fresnel-diffract
             :refer [fresnel-diffract total-energy]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-fresnel-diffract
  (testing "1D shape preserved"
    (is (= 32 (count (fresnel-diffract (vec (repeat 32 1.0)) 1 1 1)))))
  (testing "2D shape preserved"
    (let [res (fresnel-diffract (vec (repeat 8 (vec (repeat 8 1.0)))) 1 1 1)]
      (is (= 8 (count res)))
      (is (= 8 (count (first res))))))
  (testing "energy conserved in 1D"
    (let [wf0 (vec (repeat 32 1.0))
          wfz (fresnel-diffract wf0 1 1 1)]
      (is (approx= (total-energy (mapv (fn [x] [x 0.0]) wf0))
                   (total-energy wfz)
                   1e-6))))
  (testing "zero propagation returns the contact image in 1D"
    (let [wf0 (vec (map #(if (<= (Math/abs %) 8) 1.0 0.0) (range -16 16)))
          wfz (fresnel-diffract wf0 1 1 0)]
      (doseq [[a [r _]] (map vector wf0 wfz)]
        (is (approx= a r 1e-6)))))
  (testing "3D input throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (fresnel-diffract [[[1]]] 1 1 1)))))
