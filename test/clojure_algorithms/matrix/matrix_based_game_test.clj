(ns clojure-algorithms.matrix.matrix-based-game-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.matrix-based-game :as sut]))

(defn- approx
  "Tolerant equality for ported tests: exact on integers, 1e-6 on floats."
  [a b]
  (cond (and (integer? a) (integer? b)) (= a b)
        (and (number? a) (number? b))
        (let [x (double a) y (double b)]
          (or (= x y) (< (Math/abs (- x y)) 1e-6)))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map approx a b)))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? true? (map (fn [[k v]]
                                  (and (contains? b k) (approx v (get b k))))
                                a)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

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
  (is (approx #{} (sut/find-repeat [["-" "-" "-"] ["-" "-" "-"] ["-" "-" "-"]] 1 1 3))))

(deftest test-fn-increment-score-examples
  (is (approx 6 (sut/increment-score 3)))
  (is (approx 0 (sut/increment-score 0))))

(deftest test-fn-move-x-examples
  (is (approx [["-" "-"] ["-" "A"] ["-" "C"]] (sut/move-x [["-" "A"] ["-" "-"] ["-" "C"]] 1 2))))

(deftest test-fn-move-y-examples
  (is (approx [["A" "-"] ["-" "-"] ["-" "C"]] (sut/move-y [["-" "A"] ["-" "-"] ["-" "C"]] 2))))
