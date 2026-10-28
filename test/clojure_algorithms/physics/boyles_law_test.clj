(ns clojure-algorithms.physics.boyles-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.boyles-law
             :refer [boyles-law check-validity find-target-variable]]))

(deftest test-check-validity
  (testing "valid input passes"
    (is (nil? (check-validity {:v1 2 :v2 4 :p1 6}))))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (check-validity {})))
    (is (thrown? clojure.lang.ExceptionInfo (check-validity {:v1 2 :v2 4 :k 6})))
    (is (thrown? clojure.lang.ExceptionInfo (check-validity {:v1 2 :v2 4 :p1 -6})))))

(deftest test-find-target-variable
  (testing "finds the missing variable"
    (is (= :p1 (find-target-variable {:v1 1 :v2 2 :p2 4}))))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (find-target-variable {})))
    (is (thrown? clojure.lang.ExceptionInfo (find-target-variable {:v1 1 :v2 2 :k 4})))
    (is (thrown? clojure.lang.ExceptionInfo (find-target-variable {:v1 1 :v2 -2 :p2 4})))))

(deftest test-boyles-law
  (testing "known values"
    (is (= {:p2 "80.0 Pa"} (boyles-law {:p1 100 :v2 150 :v1 120})))
    (is (= {:v2 "10.0 L"} (boyles-law {:p1 10 :v1 20 :p2 20})))
    (is (= {:p1 "24.846 Pa"} (boyles-law {:v1 13 :p2 17 :v2 19})))
    (is (= {:v1 "31.32 L"} (boyles-law {:v2 27 :p1 25 :p2 29}))))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (boyles-law {:p1 2 :v2 1})))
    (is (thrown? clojure.lang.ExceptionInfo (boyles-law {})))
    (is (thrown? clojure.lang.ExceptionInfo (boyles-law {:p1 2 :v2 1 :k 6})))
    (is (thrown? clojure.lang.ExceptionInfo (boyles-law {:p1 2 :v2 1 :v1 -6})))))
