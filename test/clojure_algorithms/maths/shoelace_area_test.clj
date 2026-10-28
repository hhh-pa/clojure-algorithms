(ns clojure-algorithms.maths.shoelace-area-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.shoelace-area :as sut]))

(deftest test-fn-shoelace-area-examples
  (is (approx 6.0 (sut/shoelace-area [[0 0] [4 0] [0 3]])))
  (is (approx 1.0 (sut/shoelace-area [[0 0] [1 0] [1 1] [0 1]])))
  (is (approx 4.0 (sut/shoelace-area (reverse [[0 0] [2 0] [2 2] [0 2]]))))
  (is (approx 4.0 (sut/shoelace-area [[0 0] [2 0] [2 2] [0 2]]))))

(deftest test-fn-shoelace-area-extra
  (is (thrown? Exception (sut/shoelace-area [[0 0] [1 1]])))
  (is (thrown? Exception (sut/shoelace-area [[0 0] [1 "a"] [2 2]])))
  (is (thrown? Exception (sut/shoelace-area [[0 0] 5 [1 1]])))
  (is (= :ValueError (try (sut/shoelace-area [[0 0]]) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :TypeError (try (sut/shoelace-area [[0 0] [1 nil] [2 2]]) nil
                         (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
