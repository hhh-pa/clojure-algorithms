(ns clojure-algorithms.other.gauss-easter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.gauss-easter :as g])
  (:import [java.time LocalDate]))

(deftest gauss-easter-test
  (testing "doctest years"
    (is (= (LocalDate/of 2007 4 8) (g/gauss-easter 2007)))
    (is (= (LocalDate/of 2008 3 23) (g/gauss-easter 2008)))
    (is (= (LocalDate/of 2020 4 12) (g/gauss-easter 2020)))
    (is (= (LocalDate/of 2021 4 4) (g/gauss-easter 2021))))
  (testing "further known dates"
    (is (= (LocalDate/of 2023 4 9) (g/gauss-easter 2023)))
    (is (= (LocalDate/of 2000 4 23) (g/gauss-easter 2000)))))
