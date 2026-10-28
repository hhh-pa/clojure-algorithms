(ns clojure-algorithms.strings.jaro-winkler-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.jaro-winkler :refer [jaro-winkler]]))

(defn- approx=
  [a b]
  (< (abs (- a b)) 1e-9))

(deftest test-jaro-winkler
  (testing "doctest examples"
    (is (approx= 0.9611111111111111 (jaro-winkler "martha" "marhta")))
    (is (approx= 0.7333333333333334 (jaro-winkler "CRATE" "TRACE")))
    (is (approx= 0.0 (jaro-winkler "test" "dbdbdbdb")))
    (is (approx= 1.0 (jaro-winkler "test" "test")))
    (is (approx= 0.6363636363636364 (jaro-winkler "hello world" "HeLLo W0rlD")))
    (is (approx= 0.0 (jaro-winkler "test" "")))
    (is (approx= 0.4666666666666666 (jaro-winkler "hello" "world")))
    (is (approx= 0.4365079365079365 (jaro-winkler "hell**o" "*world"))))
  (testing "spaces and repeats"
    (is (= 0.911111 (Double/parseDouble
                     (format "%.6f" (jaro-winkler "aa " "aa")))))
    (is (= 0.9 (Double/parseDouble
                (format "%.6f" (jaro-winkler "a a" "aa")))))
    (is (= 0.722222 (Double/parseDouble
                     (format "%.6f" (jaro-winkler "aabb" "bab")))))))
