(ns clojure-algorithms.electronics.apparent-power-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.apparent-power :as ap]))

(defn- approx=
  [[a b] [c d] eps]
  (and (< (Math/abs (- a c)) eps) (< (Math/abs (- b d)) eps)))

(deftest apparent-power-test
  (testing "apparent power phasor products"
    (is (approx= [500.0 0.0] (ap/apparent-power 100 5 0 0) 1e-9))
    (is (approx= [3.061616997868383e-14 500.0]
                 (ap/apparent-power 100 5 90 0) 1e-9))
    (is (approx= [-129.40952255126027 -482.9629131445341]
                 (ap/apparent-power 100 5 -45 -60) 1e-9))
    (is (approx= [-999.9999999999998 -1732.0508075688776]
                 (ap/apparent-power 200 10 -30 -90) 1e-9))))
