(ns clojure-algorithms.physics.n-body-simulation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.n-body-simulation
             :refer [body-position body-velocity make-body make-system
                     update-position update-system update-velocity]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps] (every? true? (map #(approx= %1 %2 eps) a b)))

(deftest test-body-updates
  (testing "velocity update"
    (let [b (make-body 0 0 0 0)]
      (is (approx-vec= (body-velocity (update-velocity b 1 0 1)) [1.0 0.0] 1e-9))
      (is (approx-vec= (body-velocity (-> b (update-velocity 1 0 1)
                                          (update-velocity 1 0 1)))
                       [2.0 0.0] 1e-9)))
    (let [b (make-body 0 0 5 0)]
      (is (approx-vec= (body-velocity (update-velocity b 0 -10 10))
                       [5.0 -100.0] 1e-9))))
  (testing "position update"
    (let [b (make-body 0 0 1 0)]
      (is (approx-vec= (body-position (update-position b 1)) [1.0 0.0] 1e-9))
      (is (approx-vec= (body-position (-> b (update-position 1)
                                          (update-position 1)))
                       [2.0 0.0] 1e-9)))
    (let [b (make-body 10 10 0 -2)]
      (is (approx-vec= (body-position (update-position b 1)) [10.0 8.0] 1e-9)))))

(deftest test-update-system
  (testing "two equal bodies attract"
    (let [sys (make-system [(make-body 0 0 0 0) (make-body 10 0 0 0)])
          next (update-system sys 1)]
      (is (= 2 (count (:bodies next))))
      (is (approx-vec= (body-position (first (:bodies next))) [0.01 0.0] 1e-9))
      (is (approx-vec= (body-velocity (first (:bodies next))) [0.01 0.0] 1e-9))))
  (testing "heavier body with time factor"
    (let [sys (make-system [(make-body -10 0 0 0)
                            (make-body 10 0 0 0 :mass 4)] :gravitation-constant 1 :time-factor 10)
          next (update-system sys 1)]
      (is (approx-vec= (body-position (first (:bodies next))) [-9.0 0.0] 1e-9))
      (is (approx-vec= (body-velocity (first (:bodies next))) [0.1 0.0] 1e-9)))))
