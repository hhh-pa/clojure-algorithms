(ns clojure-algorithms.maths.mobius-function-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.mobius-function :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-mobius-examples
  (is (= 0 (sut/mobius 24)))
  (is (= 1 (sut/mobius -1)))
  (is (thrown? Exception (sut/mobius "asd")))
  (is (= 0 (sut/mobius (reduce *' 1N (repeat 400 10)))))
  (is (= 1 (sut/mobius 0.0)))
  (is (= 1 (sut/mobius -1424)))
  (is (thrown? Exception (sut/mobius [1 "2" 2.0])))
  (is (= :TypeError (err-type #(sut/mobius "asd"))))
  (is (= :TypeError (err-type #(sut/mobius [1 "2" 2.0])))))

(deftest test-mobius-extra
  (is (= 1 (sut/mobius 1)))
  (is (= -1 (sut/mobius 2)))
  (is (= -1 (sut/mobius 3)))
  (is (= 0 (sut/mobius 4)))
  (is (= 1 (sut/mobius 6)))
  (is (= 0 (sut/mobius 12)))
  (is (= -1 (sut/mobius 30))))
