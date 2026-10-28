(ns clojure-algorithms.maths.dodecahedron-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.dodecahedron :as sut]))

(deftest test-fn-dodecahedron-surface-area-examples
  (is (approx 516.1432201766901 (sut/dodecahedron-surface-area 5)))
  (is (approx 2064.5728807067603 (sut/dodecahedron-surface-area 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/dodecahedron-surface-area -1))))

(deftest test-fn-dodecahedron-volume-examples
  (is (approx 957.8898700780791 (sut/dodecahedron-volume 5)))
  (is (approx 7663.118960624633 (sut/dodecahedron-volume 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/dodecahedron-volume -1))))
