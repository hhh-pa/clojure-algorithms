(ns clojure-algorithms.maths.monte-carlo-dice-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.monte-carlo-dice :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/throw-dice))
  (is (some? #'sut/make-Dice))
  (is (some? #'sut/Dice-roll))
  (is (some? #'sut/Dice-NUM-SIDES)))
