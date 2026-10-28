(ns clojure-algorithms.maths.minkowski-distance-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.minkowski-distance :as sut]))

(deftest test-fn-minkowski-distance-examples
  (is (approx 2.0 (sut/minkowski-distance [1.0 1.0] [2.0 2.0] 1)))
  (is (approx 8.0 (sut/minkowski-distance [1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] 2)))
  (is (approx 5.0 (sut/minkowski-distance [5.0] [0.0] 3)))
  (is (thrown? Exception (sut/minkowski-distance [1.0] [2.0] -1)))
  (is (thrown? Exception (sut/minkowski-distance [1.0] [1.0 2.0] 1))))

(deftest test-fn-minkowski-distance-extra
  (is (= :ValueError (try (sut/minkowski-distance [1.0] [2.0] -1) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :ValueError (try (sut/minkowski-distance [1.0] [1.0 2.0] 1) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
