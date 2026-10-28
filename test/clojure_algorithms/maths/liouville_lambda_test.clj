(ns clojure-algorithms.maths.liouville-lambda-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.liouville-lambda :as sut]))

(deftest test-fn-liouville-lambda-examples
  (is (approx 1 (sut/liouville-lambda 10)))
  (is (approx -1 (sut/liouville-lambda 11)))
  (is (thrown? Exception (sut/liouville-lambda 0)))
  (is (thrown? Exception (sut/liouville-lambda -1)))
  (is (thrown? Exception (sut/liouville-lambda 11.0))))

(deftest test-fn-liouville-lambda-extra
  (is (approx 1 (sut/liouville-lambda 1)))
  (is (approx 1 (sut/liouville-lambda 4)))
  (is (approx -1 (sut/liouville-lambda 8)))
  (is (= :ValueError (try (sut/liouville-lambda 0)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e))))))
  (is (= :TypeError (try (sut/liouville-lambda 11.0)
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (:type (ex-data e)))))))
