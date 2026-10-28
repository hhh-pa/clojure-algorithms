(ns clojure-algorithms.maths.weighted-average-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.weighted-average :as sut]))

(deftest test-fn-weighted-average-examples
  (is (approx 2.0 (sut/weighted-average [1 2 3] [1 1 1])))
  (is (approx 23.333333333333332 (sut/weighted-average [10 20 30] [1 2 3])))
  (is (approx 12.5 (sut/weighted-average [5 15] [1 3])))
  (is (approx 100.0 (sut/weighted-average [100] [0.5])))
  (is (thrown? Exception (sut/weighted-average [] [])))
  (is (thrown? Exception (sut/weighted-average [1 2] [1])))
  (is (thrown? Exception (sut/weighted-average [1 2 3] [0 0 0]))))

(deftest test-fn-weighted-average-extra
  (is (= :ValueError (try (sut/weighted-average [] []) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :ValueError (try (sut/weighted-average [1 2] [1]) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :ValueError (try (sut/weighted-average [1 2 3] [0 0 0]) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
