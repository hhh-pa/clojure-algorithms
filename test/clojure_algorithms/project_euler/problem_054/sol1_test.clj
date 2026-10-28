(ns clojure-algorithms.project-euler.problem-054.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-054.sol1 :refer [poker-hand compare-with hand-name solution]]))

(deftest compare-with-test
  (is (= "Loss" (compare-with (poker-hand "2H 3H 4H 5H 6H")
                              (poker-hand "KS AS TS QS JS"))))
  (is (= "Win" (compare-with (poker-hand "2S AH 2H AS AC")
                             (poker-hand "2H 3H 5H 6H 7H"))))
  (is (= "Tie" (compare-with (poker-hand "2S AH 4H 5S 6C")
                             (poker-hand "AD 4C 5H 6H 2C")))))

(deftest hand-name-test
  (is (= "Royal flush" (hand-name (poker-hand "KS AS TS QS JS"))))
  (is (= "Straight flush, Six-high" (hand-name (poker-hand "2D 6D 3D 4D 5D"))))
  (is (= "Four of a kind, Jacks" (hand-name (poker-hand "JC 6H JS JD JH"))))
  (is (= "Full house, Twos over Threes" (hand-name (poker-hand "3D 2H 3H 2C 2D"))))
  (is (= "Straight, Five-high" (hand-name (poker-hand "2H 4D 3C AS 5S")))))

(deftest solution-test
  (is (= 1 (solution ["KS AS TS QS JS 2H 3H 4H 5H 6H"
                      "2H 3H 4H 5H 6H KS AS TS QS JS"]))))
