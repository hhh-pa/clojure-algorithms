(ns clojure-algorithms.maths.chudnovsky-algorithm-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.chudnovsky-algorithm :as sut]))

(deftest test-fn-pi-examples
  (is (= "3.14159265" (sut/pi 10)))
  (is (= (str "3.1415926535897932384626433832795028841971693993751058209749445923"
              "0781640628620899862803482534211706")
         (sut/pi 100)))
  (is (thrown? Exception (sut/pi "hello")))
  (is (thrown? Exception (sut/pi -1))))

(deftest test-fn-pi-errors
  (is (= :TypeError (try (sut/pi "hello")
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (:type (ex-data e))))))
  (is (= :TypeError (try (sut/pi 2.5)
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (:type (ex-data e))))))
  (is (= :ValueError (try (sut/pi -1)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e))))))
  (is (= :ValueError (try (sut/pi 0)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e)))))))
