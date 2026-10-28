(ns clojure-algorithms.maths.chebyshev-distance-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.chebyshev-distance :as sut]))

(deftest test-fn-chebyshev-distance-examples
  (is (approx 1.0 (sut/chebyshev-distance [1.0 1.0] [2.0 2.0])))
  (is (approx 14.2 (sut/chebyshev-distance [1.0 1.0 9.0] [2.0 2.0 -5.2])))
  (is (thrown? clojure.lang.ExceptionInfo
               (sut/chebyshev-distance [1.0] [2.0 2.0]))))
