(ns clojure-algorithms.maths.special-numbers.pronic-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.pronic-number :as sut]))

(deftest test-fn-is-pronic-examples
  (is (= false (boolean (sut/is-pronic -1))))
  (is (= true (boolean (sut/is-pronic 0))))
  (is (= true (boolean (sut/is-pronic 2))))
  (is (= false (boolean (sut/is-pronic 5))))
  (is (= true (boolean (sut/is-pronic 6))))
  (is (= false (boolean (sut/is-pronic 8))))
  (is (= true (boolean (sut/is-pronic 30))))
  (is (= false (boolean (sut/is-pronic 32))))
  (is (= true (boolean (sut/is-pronic 2147441940))))
  (is (= true (boolean (sut/is-pronic 9223372033963249500))))
  (is (thrown? Exception (sut/is-pronic 6.0))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/is-pronic 6.0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
