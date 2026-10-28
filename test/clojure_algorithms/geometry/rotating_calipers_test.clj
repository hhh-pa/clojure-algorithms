(ns clojure-algorithms.geometry.rotating-calipers-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.rotating-calipers
             :refer [->Point convex-hull rotating-calipers]]))

(deftest test-convex-hull
  (is (= [(->Point 0.0 0.0) (->Point 1.0 1.0)]
         (convex-hull [(->Point 0.0 0.0) (->Point 1.0 1.0)])))
  (is (= [(->Point 0.0 0.0) (->Point 2.0 2.0)]
         (convex-hull [(->Point 0.0 0.0) (->Point 1.0 1.0) (->Point 2.0 2.0)]))))

(deftest test-rotating-calipers
  (let [[d pair] (rotating-calipers [(->Point 0.0 0.0) (->Point 3.0 0.0)
                                     (->Point 3.0 4.0) (->Point 0.0 4.0)])]
    (is (= 5.0 d))
    (is (contains? #{[(->Point 0.0 0.0) (->Point 3.0 4.0)]
                     [(->Point 3.0 4.0) (->Point 0.0 0.0)]
                     [(->Point 3.0 0.0) (->Point 0.0 4.0)]
                     [(->Point 0.0 4.0) (->Point 3.0 0.0)]}
                   (vec pair))))
  (is (= [5.0 [(->Point 0.0 0.0) (->Point 0.0 5.0)]]
         (rotating-calipers [(->Point 0.0 0.0) (->Point 0.0 5.0)])))
  (is (< (Math/abs (- (first (rotating-calipers [(->Point 0.0 0.0)
                                                 (->Point 1.0 1.0)
                                                 (->Point 2.0 2.0)
                                                 (->Point 3.0 3.0)]))
                      4.242640687119285)) 1e-9))
  (is (thrown? IllegalArgumentException
               (rotating-calipers [(->Point 1.0 1.0)]))))
