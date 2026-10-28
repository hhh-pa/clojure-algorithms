(ns clojure-algorithms.maths.perfect-square-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.perfect-square :as sut]))

(deftest test-fn-perfect-square-examples
  (is (= true (sut/perfect-square 9)))
  (is (= true (sut/perfect-square 16)))
  (is (= true (sut/perfect-square 1)))
  (is (= true (sut/perfect-square 0)))
  (is (= false (sut/perfect-square 10))))

(deftest test-fn-perfect-square-binary-search-examples
  (is (= true (sut/perfect-square-binary-search 9)))
  (is (= true (sut/perfect-square-binary-search 16)))
  (is (= true (sut/perfect-square-binary-search 1)))
  (is (= true (sut/perfect-square-binary-search 0)))
  (is (= false (sut/perfect-square-binary-search 10)))
  (is (= false (sut/perfect-square-binary-search -1)))
  (is (= false (sut/perfect-square-binary-search 1.1)))
  (is (thrown? Exception (sut/perfect-square-binary-search "a")))
  ;; Re-applied quarantined assertion: upstream raises TypeError for None.
  (is (thrown? Exception (sut/perfect-square-binary-search nil)))
  (is (thrown? Exception (sut/perfect-square-binary-search []))))
