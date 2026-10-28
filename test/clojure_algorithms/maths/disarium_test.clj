(ns clojure-algorithms.maths.disarium-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.disarium :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-is-disarium-examples
  (is (= true (boolean (sut/is-disarium 89))))
  (is (= false (boolean (sut/is-disarium 75))))
  (is (= true (boolean (sut/is-disarium 135)))))

(deftest test-is-disarium-extra
  (is (true? (sut/is-disarium 1)))
  (is (true? (sut/is-disarium 175)))
  (is (true? (sut/is-disarium 518)))
  (is (false? (sut/is-disarium 100)))
  (is (false? (sut/is-disarium 10)))
  (is (= :ValueError (err-type #(sut/is-disarium -89))))
  (is (= :ValueError (err-type #(sut/is-disarium 8.5)))))
