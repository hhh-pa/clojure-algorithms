(ns clojure-algorithms.maths.volume-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.volume :as sut]))

(deftest test-fn-vol-cube-examples
  (is (approx 1.0 (sut/vol-cube 1)))
  (is (approx 27.0 (sut/vol-cube 3)))
  (is (approx 0.0 (sut/vol-cube 0)))
  (is (approx 4.096000000000001 (sut/vol-cube 1.6)))
  (is (thrown? Exception (sut/vol-cube -1))))

(deftest test-fn-vol-spherical-cap-examples
  (is (approx 5.235987755982988 (sut/vol-spherical-cap 1 2)))
  (is (approx 16.621119532592402 (sut/vol-spherical-cap 1.6 2.6)))
  (is (approx 0.0 (sut/vol-spherical-cap 0 0)))
  (is (thrown? Exception (sut/vol-spherical-cap -1 2)))
  (is (thrown? Exception (sut/vol-spherical-cap 1 -2))))

(deftest test-fn-vol-spheres-intersect-examples
  (is (approx 21.205750411731103 (sut/vol-spheres-intersect 2 2 1)))
  (is (approx 40.71504079052372 (sut/vol-spheres-intersect 2.6 2.6 1.6)))
  (is (approx 0.0 (sut/vol-spheres-intersect 0 0 0)))
  (is (thrown? Exception (sut/vol-spheres-intersect -2 2 1)))
  (is (thrown? Exception (sut/vol-spheres-intersect 2 -2 1)))
  (is (thrown? Exception (sut/vol-spheres-intersect 2 2 -1))))

(deftest test-fn-vol-spheres-union-examples
  (is (approx 45.814892864851146 (sut/vol-spheres-union 2 2 1)))
  (is (approx 48.77802773671288 (sut/vol-spheres-union 1.56 2.2 1.4)))
  (is (thrown? Exception (sut/vol-spheres-union 0 2 1)))
  (is (thrown? Exception (sut/vol-spheres-union "1.56" "2.2" "1.4")))
  (is (thrown? Exception (sut/vol-spheres-union 1 nil 1))))

(deftest test-fn-vol-cuboid-examples
  (is (approx 1.0 (sut/vol-cuboid 1 1 1)))
  (is (approx 6.0 (sut/vol-cuboid 1 2 3)))
  (is (approx 14.976 (sut/vol-cuboid 1.6 2.6 3.6)))
  (is (approx 0.0 (sut/vol-cuboid 0 0 0)))
  (is (thrown? Exception (sut/vol-cuboid -1 2 3)))
  (is (thrown? Exception (sut/vol-cuboid 1 -2 3)))
  (is (thrown? Exception (sut/vol-cuboid 1 2 -3))))

(deftest test-fn-vol-cone-examples
  (is (approx 10.0 (sut/vol-cone 10 3)))
  (is (approx 0.3333333333333333 (sut/vol-cone 1 1)))
  (is (approx 0.8533333333333335 (sut/vol-cone 1.6 1.6)))
  (is (approx 0.0 (sut/vol-cone 0 0)))
  (is (thrown? Exception (sut/vol-cone -1 1)))
  (is (thrown? Exception (sut/vol-cone 1 -1))))

(deftest test-fn-vol-right-circ-cone-examples
  (is (approx 12.566370614359172 (sut/vol-right-circ-cone 2 3)))
  (is (approx 0.0 (sut/vol-right-circ-cone 0 0)))
  (is (approx 4.289321169701265 (sut/vol-right-circ-cone 1.6 1.6)))
  (is (thrown? Exception (sut/vol-right-circ-cone -1 1)))
  (is (thrown? Exception (sut/vol-right-circ-cone 1 -1))))

(deftest test-fn-vol-prism-examples
  (is (approx 20.0 (sut/vol-prism 10 2)))
  (is (approx 11.0 (sut/vol-prism 11 1)))
  (is (approx 2.5600000000000005 (sut/vol-prism 1.6 1.6)))
  (is (approx 0.0 (sut/vol-prism 0 0)))
  (is (thrown? Exception (sut/vol-prism -1 1)))
  (is (thrown? Exception (sut/vol-prism 1 -1))))

(deftest test-fn-vol-pyramid-examples
  (is (approx 10.0 (sut/vol-pyramid 10 3)))
  (is (approx 1.5 (sut/vol-pyramid 1.5 3)))
  (is (approx 0.8533333333333335 (sut/vol-pyramid 1.6 1.6)))
  (is (approx 0.0 (sut/vol-pyramid 0 0)))
  (is (thrown? Exception (sut/vol-pyramid -1 1)))
  (is (thrown? Exception (sut/vol-pyramid 1 -1))))

(deftest test-fn-vol-sphere-examples
  (is (approx 523.5987755982989 (sut/vol-sphere 5)))
  (is (approx 4.1887902047863905 (sut/vol-sphere 1)))
  (is (approx 17.15728467880506 (sut/vol-sphere 1.6)))
  (is (approx 0.0 (sut/vol-sphere 0)))
  (is (thrown? Exception (sut/vol-sphere -1))))

(deftest test-fn-vol-hemisphere-examples
  (is (approx 2.0943951023931953 (sut/vol-hemisphere 1)))
  (is (approx 718.377520120866 (sut/vol-hemisphere 7)))
  (is (approx 8.57864233940253 (sut/vol-hemisphere 1.6)))
  (is (approx 0.0 (sut/vol-hemisphere 0)))
  (is (thrown? Exception (sut/vol-hemisphere -1))))

(deftest test-fn-vol-circular-cylinder-examples
  (is (approx 3.141592653589793 (sut/vol-circular-cylinder 1 1)))
  (is (approx 150.79644737231007 (sut/vol-circular-cylinder 4 3)))
  (is (approx 12.867963509103795 (sut/vol-circular-cylinder 1.6 1.6)))
  (is (approx 0.0 (sut/vol-circular-cylinder 0 0)))
  (is (thrown? Exception (sut/vol-circular-cylinder -1 1)))
  (is (thrown? Exception (sut/vol-circular-cylinder 1 -1))))

(deftest test-fn-vol-hollow-circular-cylinder-examples
  (is (approx 28.274333882308138 (sut/vol-hollow-circular-cylinder 1 2 3)))
  (is (approx 47.50088092227767 (sut/vol-hollow-circular-cylinder 1.6 2.6 3.6)))
  (is (thrown? Exception (sut/vol-hollow-circular-cylinder -1 2 3)))
  (is (thrown? Exception (sut/vol-hollow-circular-cylinder 1 -2 3)))
  (is (thrown? Exception (sut/vol-hollow-circular-cylinder 1 2 -3)))
  (is (thrown? Exception (sut/vol-hollow-circular-cylinder 2 1 3)))
  (is (thrown? Exception (sut/vol-hollow-circular-cylinder 0 0 0))))

(deftest test-fn-vol-conical-frustum-examples
  (is (approx 48490.482608158454 (sut/vol-conical-frustum 45 7 28)))
  (is (approx 7.330382858376184 (sut/vol-conical-frustum 1 1 2)))
  (is (approx 48.7240076620753 (sut/vol-conical-frustum 1.6 2.6 3.6)))
  (is (approx 0.0 (sut/vol-conical-frustum 0 0 0)))
  (is (thrown? Exception (sut/vol-conical-frustum -2 2 1)))
  (is (thrown? Exception (sut/vol-conical-frustum 2 -2 1)))
  (is (thrown? Exception (sut/vol-conical-frustum 2 2 -1))))

(deftest test-fn-vol-torus-examples
  (is (approx 19.739208802178716 (sut/vol-torus 1 1)))
  (is (approx 710.6115168784338 (sut/vol-torus 4 3)))
  (is (approx 947.4820225045784 (sut/vol-torus 3 4)))
  (is (approx 80.85179925372404 (sut/vol-torus 1.6 1.6)))
  (is (approx 0.0 (sut/vol-torus 0 0)))
  (is (thrown? Exception (sut/vol-torus -1 1)))
  (is (thrown? Exception (sut/vol-torus 1 -1))))

(deftest test-fn-vol-octahedron-examples
  (is (approx 7.36569563735987 (sut/vol-octahedron 2.5)))
  (is (approx 471.40452079103168 (sut/vol-octahedron 10)))
  (is (approx 58.92556509887896 (sut/vol-octahedron 5)))
  (is (approx 20.21146882891548 (sut/vol-octahedron 3.5)))
  (is (approx 0.0 (sut/vol-octahedron 0)))
  (is (thrown? Exception (sut/vol-octahedron -1))))

(deftest test-fn-vol-icosahedron-examples
  (is (approx 34.088984228514256 (sut/vol-icosahedron 2.5)))
  (is (approx 2181.694990624912374 (sut/vol-icosahedron 10)))
  (is (approx 272.711873828114047 (sut/vol-icosahedron 5)))
  (is (approx 92.740688412033628 (sut/vol-icosahedron 3.49)))
  (is (approx 0.0 (sut/vol-icosahedron 0)))
  (is (thrown? Exception (sut/vol-icosahedron -1)))
  (is (thrown? Exception (sut/vol-icosahedron -0.2))))
