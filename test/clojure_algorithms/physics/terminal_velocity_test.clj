(ns clojure-algorithms.physics.terminal-velocity-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.terminal-velocity :refer [terminal-velocity]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-terminal-velocity
  (testing "known values"
    (is (approx= (terminal-velocity 1 25 0.6 0.77) 1.3031197996044768 1e-12))
    (is (approx= (terminal-velocity 2 100 0.45 0.23) 1.9467947148674276 1e-12))
    (is (approx= (terminal-velocity 5 50 0.2 0.5) 4.428690551393267 1e-12)))
  (testing "non-positive inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (terminal-velocity -5 50 -0.2 -2)))
    (is (thrown? clojure.lang.ExceptionInfo (terminal-velocity 3 -20 -1 2)))
    (is (thrown? clojure.lang.ExceptionInfo (terminal-velocity -2 -1 -0.44 -1)))))
