(ns clojure-algorithms.maths.derangement-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.derangement :as sut]))

(deftest test-fn-derangement-examples
  (is (approx 2 (sut/derangement 3)))
  (is (approx 44 (sut/derangement 5)))
  (is (approx 1334961 (sut/derangement 10))))

(deftest test-fn-derangement-extra
  (is (approx 0 (sut/derangement 0)))
  (is (approx 0 (sut/derangement 1)))
  (is (approx 1 (sut/derangement 2)))
  (is (approx 9 (sut/derangement 4)))
  (is (thrown? Exception (sut/derangement -1)))
  (is (= :ValueError (try (sut/derangement -1)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e)))))))
