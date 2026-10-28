(ns clojure-algorithms.maths.bearing-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.bearing :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defn- round3 [x] (/ (Math/round (* x 1000.0)) 1000.0))

(deftest test-fn-initial-bearing-examples
  (is (approx 9.12 (round3 (sut/initial-bearing [50.066389 -5.714722]
                                                [58.643889 -3.07]))))
  (is (approx 44.996 (round3 (sut/initial-bearing [0.0 0.0] [1.0 1.0]))))
  (is (thrown? Exception (sut/initial-bearing [0.0 0.0] [0.0 0.0])))
  (is (= :ValueError (err-type #(sut/initial-bearing [0.0 0.0] [0.0 0.0])))))

(deftest test-initial-bearing-types
  (is (= :TypeError (err-type #(sut/initial-bearing ["a" "b"] [1.0 1.0]))))
  (is (= :TypeError (err-type #(sut/initial-bearing [0.0] [1.0 1.0]))))
  (is (= :TypeError (err-type #(sut/initial-bearing 5 [1.0 1.0]))))
  (is (approx 9.119817327410374
              (sut/initial-bearing [50.066389 -5.714722] [58.643889 -3.07])))
  (is (approx 44.99563645534488 (sut/initial-bearing [0.0 0.0] [1.0 1.0])))
  (let [b (sut/initial-bearing [10.0 20.0] [30.0 40.0])]
    (is (and (<= 0.0 b) (< b 360.0)))))
