(ns clojure-algorithms.maths.numerical-analysis.simpson-rule-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure-algorithms.maths.numerical-analysis.simpson-rule :as sut]))

(defn- py-round
  [v n]
  (Double/parseDouble
   (.toPlainString (.setScale (BigDecimal. ^double (double v)) n
                              java.math.RoundingMode/HALF_EVEN))))

(defn- ex-type
  [thunk]
  (try (thunk) :no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-method-2-examples
  (is (= 2.6666666667 (py-round (sut/method-2 [0 2 4] 10) 10)))
  (is (= -0.2666666667 (py-round (sut/method-2 [2 0] 10) 10)))
  (is (= 2.172 (py-round (sut/method-2 [-2 -1] 10) 10)))
  (is (= 0.3333333333 (py-round (sut/method-2 [0 1] 10) 10)))
  (is (= 2.6666666667 (py-round (sut/method-2 [0 2] 10) 10)))
  (is (= 2.5621226667 (py-round (sut/method-2 [0 2] 100) 10)))
  (is (= 0.3320026653 (py-round (sut/method-2 [0 1] 1000) 10)))
  (is (= :ZeroDivisionError (ex-type #(sut/method-2 [0 2] 0))))
  (is (= :ZeroDivisionError (ex-type #(sut/method-2 [0 2] -10)))))

(deftest test-fn-make-points-examples
  (is (approx [0.2 0.4 0.6000000000000001 0.8 1.0 1.2 1.4 1.5999999999999999
               1.7999999999999998]
              (sut/make-points 0 2 0.2)))
  (is (= [] (sut/make-points 2 0 -0.2))))

(deftest test-fn-f-examples
  (is (= 9 (sut/f 3)))
  (is (= 0 (sut/f 0))))

(deftest test-fn-main-examples
  (is (let [out (with-out-str (sut/main))]
        (and (clojure.string/starts-with? out "y = 0.3333333333")
             (clojure.string/ends-with? out "\n")))))
