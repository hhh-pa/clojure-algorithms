(ns clojure-algorithms.matrix.matrix-based-game-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.matrix-based-game :as sut]))

(deftest test-fn-validate-matrix-size-examples
  (is (thrown? Exception (sut/validate-matrix-size -1))))

(deftest test-fn-validate-matrix-content-examples
  (is (thrown? Exception (sut/validate-matrix-content ["aaaa" "aaaa" "aaaa" "aaaa"] 3)))
  (is (thrown? Exception (sut/validate-matrix-content ["aa%" "aaa" "aaa"] 3)))
  (is (thrown? Exception (sut/validate-matrix-content ["aaa" "aaa" "aaaa"] 3))))

(deftest test-fn-validate-moves-examples
  (is (thrown? Exception (sut/validate-moves [[1 2] [-1 0]] 3))))

(deftest test-fn-parse-moves-examples
  (is (approx [[0 1] [1 1]] (sut/parse-moves "0 1, 1 1")))
  (is (thrown? Exception (sut/parse-moves "0 1, 1 1, 2")))
  (is (thrown? Exception (sut/parse-moves "0 1, 1 1, 2 4 5 6"))))

(deftest test-fn-find-repeat-examples
  (is (approx #{[1 2] [2 1] [0 0] [2 0] [0 2] [2 2] [1 0]}
              (sut/find-repeat [["A" "B" "A"] ["A" "B" "A"] ["A" "A" "A"]] 0 0 3)))
  (is (approx #{} (sut/find-repeat [["-" "-" "-"] ["-" "-" "-"] ["-" "-" "-"]] 1 1 3))))

(deftest test-fn-increment-score-examples
  (is (approx 6 (sut/increment-score 3)))
  (is (approx 0 (sut/increment-score 0))))

(deftest test-fn-move-x-examples
  (is (approx [["-" "-"] ["-" "A"] ["-" "C"]] (sut/move-x [["-" "A"] ["-" "-"] ["-" "C"]] 1 2))))

(deftest test-fn-move-y-examples
  (is (approx [["A" "-"] ["-" "-"] ["-" "C"]] (sut/move-y [["-" "A"] ["-" "-"] ["-" "C"]] 2))))

(deftest test-fn-play-examples
  (is (approx [[["G" "-"] ["G" "-"]] 3] (sut/play [["R" "G"] ["R" "G"]] 0 0 2))))

(deftest test-fn-process-game-examples
  (is (approx 6 (sut/process-game 3 ["aaa" "bbb" "ccc"] [[0 0]]))))
