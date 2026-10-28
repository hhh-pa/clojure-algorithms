(ns clojure-algorithms.maths.keith-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.keith-number :as sut]))

(deftest test-fn-is-keith-number-examples
  (is (= true (boolean (sut/is-keith-number 14))))
  (is (= true (boolean (sut/is-keith-number 19))))
  (is (= true (boolean (sut/is-keith-number 28))))
  (is (= true (boolean (sut/is-keith-number 47))))
  (is (= true (boolean (sut/is-keith-number 61))))
  (is (= true (boolean (sut/is-keith-number 75))))
  (is (= true (boolean (sut/is-keith-number 197))))
  (is (= true (boolean (sut/is-keith-number 742))))
  (is (= true (boolean (sut/is-keith-number 1104))))
  (is (= true (boolean (sut/is-keith-number 1537))))
  (is (= false (boolean (sut/is-keith-number 10))))
  (is (= false (boolean (sut/is-keith-number 12))))
  (is (= false (boolean (sut/is-keith-number 25))))
  (is (= false (boolean (sut/is-keith-number 100))))
  (is (= false (boolean (sut/is-keith-number 9))))
  (is (= false (boolean (sut/is-keith-number 1))))
  (is (= false (boolean (sut/is-keith-number 0))))
  (is (= false (boolean (sut/is-keith-number -14)))))

(deftest test-fn-is-keith-number-errors
  (is (thrown? Exception (sut/is-keith-number 14.0)))
  (is (thrown? Exception (sut/is-keith-number "14")))
  (is (thrown? Exception (sut/is-keith-number true)))
  (is (thrown? Exception (sut/is-keith-number nil)))
  (is (= :TypeError (try (sut/is-keith-number 14.0)
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (:type (ex-data e)))))))

(deftest test-fn-find-keith-numbers-examples
  (is (approx [14 19 28 47 61 75] (sut/find-keith-numbers 100)))
  (is (approx [14 19 28 47 61 75 197] (sut/find-keith-numbers 200)))
  (is (approx [] (sut/find-keith-numbers 10)))
  (is (thrown? Exception (sut/find-keith-numbers 9)))
  (is (thrown? Exception (sut/find-keith-numbers 10.5)))
  (is (thrown? Exception (sut/find-keith-numbers false)))
  (is (= :ValueError (try (sut/find-keith-numbers 9)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e))))))
  (is (= :TypeError (try (sut/find-keith-numbers 10.5)
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (:type (ex-data e)))))))
