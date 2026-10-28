(ns clojure-algorithms.maths.special-numbers.harshad-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.harshad-numbers :as sut]))

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

(deftest test-fn-int-to-base-examples
  (is (= "0" (str (sut/int-to-base 0 21))))
  (is (approx "10111" (sut/int-to-base 23 2)))
  (is (approx "213" (sut/int-to-base 58 5)))
  (is (approx "A7" (sut/int-to-base 167 16)))
  (is (thrown? Exception (sut/int-to-base 98 1)))
  (is (thrown? Exception (sut/int-to-base 98 37)))
  (is (thrown? Exception (sut/int-to-base -99 16))))

(deftest test-fn-sum-of-digits-examples
  (is (approx "13" (sut/sum-of-digits 103 12)))
  (is (approx "30" (sut/sum-of-digits 1275 4)))
  (is (approx "1001" (sut/sum-of-digits 6645 2)))
  (is (thrown? Exception (sut/sum-of-digits 543 1)))
  (is (thrown? Exception (sut/sum-of-digits 543 37))))

(deftest test-fn-harshad-numbers-in-base-examples
  (is (approx ["1" "10" "100" "110" "1000" "1010" "1100"] (sut/harshad-numbers-in-base 15 2)))
  (is (approx ["1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B"] (sut/harshad-numbers-in-base 12 34)))
  (is (approx ["1" "2" "3" "10" "12" "20" "21"] (sut/harshad-numbers-in-base 12 4)))
  (is (thrown? Exception (sut/harshad-numbers-in-base 234 37)))
  (is (thrown? Exception (sut/harshad-numbers-in-base 234 1)))
  (is (approx [] (sut/harshad-numbers-in-base -12 6))))

(deftest test-fn-is-harshad-number-in-base-examples
  (is (= true (boolean (sut/is-harshad-number-in-base 18 10))))
  (is (= true (boolean (sut/is-harshad-number-in-base 21 10))))
  (is (= false (boolean (sut/is-harshad-number-in-base -21 5))))
  (is (thrown? Exception (sut/is-harshad-number-in-base 45 37)))
  (is (thrown? Exception (sut/is-harshad-number-in-base 45 1))))
