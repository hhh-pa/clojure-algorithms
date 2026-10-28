(ns clojure-algorithms.maths.largest-of-very-large-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.largest-of-very-large-numbers :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-res-examples
  (is (approx 4.892790030352132 (sut/res 5 7)))
  (is (approx 0 (sut/res 0 5)))
  (is (approx 1 (sut/res 3 0)))
  (is (thrown? Exception (sut/res -1 5)))
  (is (= :ValueError (err-type #(sut/res -1 5)))))

(deftest test-res-extra
  (is (approx 0 (sut/res 0 0)))
  (is (approx 1 (sut/res 5 0)))
  (is (> (sut/res 5 7) (sut/res 4 7))))
