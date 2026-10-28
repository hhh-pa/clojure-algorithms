(ns clojure-algorithms.physics.in-static-equilibrium-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.in-static-equilibrium
             :refer [in-static-equilibrium? polar-force]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-polar-force
  (testing "degree mode"
    (let [[fx fy] (polar-force 10 45)]
      (is (approx= fx 7.071067811865477 1e-9))
      (is (approx= fy 7.071067811865477 1e-9))))
  (testing "radian mode"
    (let [[fx fy] (polar-force 10 3.14 true)]
      (is (approx= fx -9.999987317275396 1e-9))
      (is (approx= fy 0.01592652916486828 1e-9)))))

(deftest test-in-static-equilibrium?
  (testing "unbalanced system"
    (is (false? (in-static-equilibrium? [[1 1] [-1 2]] [[1 0] [10 0]]))))
  (testing "balanced textbook problems"
    (let [forces [(polar-force 718.4 (- 180 30))
                  (polar-force 879.54 45)
                  (polar-force 100 -90)]
          loc [[0 0] [0 0] [0 0]]]
      (is (true? (in-static-equilibrium? forces loc))))
    (let [forces [(polar-force (* 30 9.81) 15)
                  (polar-force 215 (- 180 45))
                  (polar-force 264 (- 90 30))]
          loc [[0 0] [0 0] [0 0]]]
      (is (true? (in-static-equilibrium? forces loc))))
    (let [forces [[0 -2000] [0 -1200] [0 15600] [0 -12400]]
          loc [[0 0] [6 0] [10 0] [12 0]]]
      (is (true? (in-static-equilibrium? forces loc))))))
