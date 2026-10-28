(ns clojure-algorithms.maths.average-absolute-deviation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.average-absolute-deviation :as sut]))

(deftest test-fn-average-absolute-deviation-examples
  (is (approx 0.0 (sut/average-absolute-deviation [0])))
  (is (approx 1.0 (sut/average-absolute-deviation [4 1 3 2])))
  (is (approx 20.0 (sut/average-absolute-deviation [2 70 6 50 20 8 4 0])))
  (is (approx 16.25 (sut/average-absolute-deviation [-20 0 30 15])))
  (is (thrown? Exception (sut/average-absolute-deviation []))))

(deftest test-fn-average-absolute-deviation-extra
  (is (= :ValueError (try (sut/average-absolute-deviation []) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (approx 0.0 (sut/average-absolute-deviation [5 5 5]))))
