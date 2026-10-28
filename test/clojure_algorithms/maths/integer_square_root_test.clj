(ns clojure-algorithms.maths.integer-square-root-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.integer-square-root :as sut]))

(deftest test-fn-integer-square-root-examples
  (is (approx 25 (sut/integer-square-root 625)))
  (is (approx 46340 (sut/integer-square-root 2147483647)))
  (is (thrown? Exception (sut/integer-square-root -1)))
  (is (thrown? Exception (sut/integer-square-root 1.5)))
  (is (thrown? Exception (sut/integer-square-root "0"))))

(deftest test-fn-integer-square-root-extra
  (is (approx [0 1 1 1 2 2 2 2 2 3 3 3 3 3 3 3 4 4]
              (mapv sut/integer-square-root (range 18))))
  (is (every? (fn [i]
                (= (sut/integer-square-root i)
                   (long (Math/floor (Math/sqrt i)))))
              (range 20)))
  (is (= :ValueError (try (sut/integer-square-root -1) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
