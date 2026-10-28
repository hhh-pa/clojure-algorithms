(ns clojure-algorithms.maths.lucas-lehmer-primality-test-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.lucas-lehmer-primality-test :as sut]))

(deftest test-fn-lucas-lehmer-test-examples
  (is (= true (boolean (sut/lucas-lehmer-test 7))))
  (is (= false (boolean (sut/lucas-lehmer-test 11)))))

(deftest test-fn-lucas-lehmer-test-extra
  (is (= true (boolean (sut/lucas-lehmer-test 2))))
  (is (= true (boolean (sut/lucas-lehmer-test 3))))
  (is (= true (boolean (sut/lucas-lehmer-test 5))))
  (is (= true (boolean (sut/lucas-lehmer-test 13))))
  (is (= false (boolean (sut/lucas-lehmer-test 4))))
  (is (thrown? Exception (sut/lucas-lehmer-test 1)))
  (is (= :ValueError (try (sut/lucas-lehmer-test 1) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
