(ns clojure-algorithms.maths.special-numbers.kaprekar-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.kaprekar-number :as sut]))

(deftest test-fn-is-kaprekar-examples
  (is (= true (boolean (sut/is-kaprekar 1))))
  (is (= true (boolean (sut/is-kaprekar 9))))
  (is (= true (boolean (sut/is-kaprekar 45))))
  (is (= true (boolean (sut/is-kaprekar 55))))
  (is (= false (boolean (sut/is-kaprekar 10))))
  (is (= false (boolean (sut/is-kaprekar 0))))
  (is (= false (boolean (sut/is-kaprekar -1))))
  (is (= true (boolean (sut/is-kaprekar 2223))))
  (is (thrown? Exception (sut/is-kaprekar 1.5))))

(deftest test-more-kaprekar-numbers
  (is (true? (sut/is-kaprekar 99)))
  (is (true? (sut/is-kaprekar 297)))
  (is (false? (sut/is-kaprekar 100))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/is-kaprekar 1.5)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
