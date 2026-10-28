(ns clojure-algorithms.strings.wave-string-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.wave-string :refer [wave]]))

(deftest test-wave
  (testing "doctest examples"
    (is (= ["Cat" "cAt" "caT"] (wave "cat")))
    (is (= ["One" "oNe" "onE"] (wave "one")))
    (is (= ["Book" "bOok" "boOk" "booK"] (wave "book"))))
  (testing "non-letters skipped"
    (is (= ["A b" "a B"] (wave "a b")))))
