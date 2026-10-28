(ns clojure-algorithms.maths.twin-prime-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.twin-prime :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-twin-prime-examples
  (is (= 5 (sut/twin-prime 3)))
  (is (= -1 (sut/twin-prime 4)))
  (is (= 7 (sut/twin-prime 5)))
  (is (= 19 (sut/twin-prime 17)))
  (is (= -1 (sut/twin-prime 0)))
  (is (thrown? Exception (sut/twin-prime 6.0)))
  (is (= :TypeError (err-type #(sut/twin-prime 6.0))))
  (is (= :TypeError (err-type #(sut/twin-prime "3")))))

(deftest test-twin-prime-extra
  (is (= 13 (sut/twin-prime 11)))
  (is (= -1 (sut/twin-prime 1)))
  (is (= -1 (sut/twin-prime 2)))
  (is (= -1 (sut/twin-prime 23)))
  (is (= :ValueError (err-type #(sut/twin-prime -5)))))
