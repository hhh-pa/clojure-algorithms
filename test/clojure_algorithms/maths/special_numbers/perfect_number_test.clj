(ns clojure-algorithms.maths.special-numbers.perfect-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.perfect-number :as sut]))

(deftest test-fn-perfect-examples
  (is (= false (boolean (sut/perfect 27))))
  (is (= true (boolean (sut/perfect 28))))
  (is (= false (boolean (sut/perfect 29))))
  (is (= true (boolean (sut/perfect 6))))
  (is (= false (boolean (sut/perfect 12))))
  (is (= true (boolean (sut/perfect 496))))
  (is (= true (boolean (sut/perfect 8128))))
  (is (= false (boolean (sut/perfect 0))))
  (is (= false (boolean (sut/perfect -1))))
  (is (thrown? Exception (sut/perfect 12.34)))
  (is (thrown? Exception (sut/perfect "Hello"))))

(deftest test-error-type
  (is (= :ValueError
         (try (sut/perfect 12.34)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
