(ns clojure-algorithms.maths.zellers-congruence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.zellers-congruence :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-zeller-examples
  (is (= "Your date 01-31-2010, is a Sunday!" (sut/zeller "01-31-2010")))
  (is (thrown? Exception (sut/zeller "13-31-2010")))
  (is (thrown? Exception (sut/zeller ".2-31-2010")))
  (is (thrown? Exception (sut/zeller "01-33-2010")))
  (is (thrown? Exception (sut/zeller "01-.4-2010")))
  (is (thrown? Exception (sut/zeller "01-31*2010")))
  (is (thrown? Exception (sut/zeller "01^31-2010")))
  (is (thrown? Exception (sut/zeller "01-31-8999")))
  (is (thrown? Exception (sut/zeller)))
  (is (thrown? Exception (sut/zeller "")))
  (is (thrown? Exception (sut/zeller "01-31-19082939")))
  (is (= :ValueError (err-type #(sut/zeller "13-31-2010"))))
  (is (= :ValueError (err-type #(sut/zeller ".2-31-2010"))))
  (is (= :ValueError (err-type #(sut/zeller "01-33-2010"))))
  (is (= :ValueError (err-type #(sut/zeller "01-.4-2010"))))
  (is (= :ValueError (err-type #(sut/zeller "01-31*2010"))))
  (is (= :ValueError (err-type #(sut/zeller "01^31-2010"))))
  (is (= :ValueError (err-type #(sut/zeller "01-31-8999"))))
  (is (= :TypeError (err-type #(sut/zeller))))
  (is (= :ValueError (err-type #(sut/zeller ""))))
  (is (= :ValueError (err-type #(sut/zeller "01-31-19082939")))))

(deftest test-zeller-extra
  (is (= "Your date 12/25/2023, is a Monday!" (sut/zeller "12/25/2023")))
  (is (= "Your date 02-29-2024, is a Thursday!" (sut/zeller "02-29-2024")))
  (is (= "Your date 07-04-1776, is a Thursday!" (sut/zeller "07-04-1776")))
  (is (= :ValueError (err-type #(sut/zeller "02-30-2024")))))
