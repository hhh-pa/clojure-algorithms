(ns clojure-algorithms.maths.special-numbers.automorphic-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.automorphic-number :as sut]))

(deftest test-fn-is-automorphic-number-examples
  (is (= false (boolean (sut/is-automorphic-number -1))))
  (is (= true (boolean (sut/is-automorphic-number 0))))
  (is (= true (boolean (sut/is-automorphic-number 5))))
  (is (= true (boolean (sut/is-automorphic-number 6))))
  (is (= false (boolean (sut/is-automorphic-number 7))))
  (is (= true (boolean (sut/is-automorphic-number 25))))
  (is (= true (boolean (sut/is-automorphic-number 259918212890625))))
  (is (= false (boolean (sut/is-automorphic-number 259918212890636))))
  (is (= true (boolean (sut/is-automorphic-number 740081787109376))))
  (is (thrown? Exception (sut/is-automorphic-number 5.0))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/is-automorphic-number 5.0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
