(ns clojure-algorithms.geodesy.lamberts-ellipsoidal-distance-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geodesy.lamberts-ellipsoidal-distance
             :refer [lamberts-ellipsoidal-distance]]))

(defn- approx? [a b tol] (< (Math/abs (- a b)) tol))

(deftest test-lamberts-ellipsoidal-distance
  (is (approx? (lamberts-ellipsoidal-distance 37.774856 -122.424227
                                              37.864742 -119.537521)
               254032 1.0))
  (is (approx? (lamberts-ellipsoidal-distance 37.774856 -122.424227
                                              40.713019 -74.012647)
               4133295 1.0))
  (is (thrown? IllegalArgumentException
               (lamberts-ellipsoidal-distance 100 0 0 0)))
  (is (thrown? IllegalArgumentException
               (lamberts-ellipsoidal-distance 0 0 -100 0)))
  (is (thrown? IllegalArgumentException
               (lamberts-ellipsoidal-distance 0 200 0 0)))
  (is (thrown? IllegalArgumentException
               (lamberts-ellipsoidal-distance 0 0 0 -200))))
