(ns clojure-algorithms.maths.fast-inverse-sqrt-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.fast-inverse-sqrt :as sut]))

(deftest test-fn-fast-inverse-sqrt-examples
  (is (approx 0.3156857923527257 (sut/fast-inverse-sqrt 10)))
  (is (approx 0.49915357479239103 (sut/fast-inverse-sqrt 4)))
  (is (approx 0.4932849504615651 (sut/fast-inverse-sqrt 4.1)))
  (is (thrown? Exception (sut/fast-inverse-sqrt 0)))
  (is (thrown? Exception (sut/fast-inverse-sqrt -1))))

(deftest test-fn-fast-inverse-sqrt-extra
  (is (every? (fn [i]
                (let [actual (sut/fast-inverse-sqrt i)
                      expected (/ 1.0 (Math/sqrt i))]
                  (< (/ (Math/abs (- actual expected)) expected) 0.00132)))
              (range 50 60)))
  (is (= :ValueError (try (sut/fast-inverse-sqrt 0) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
