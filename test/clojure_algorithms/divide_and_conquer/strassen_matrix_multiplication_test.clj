(ns clojure-algorithms.divide-and-conquer.strassen-matrix-multiplication-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.strassen-matrix-multiplication :as s]))

(defn- naive
  "Reference naive multiplication."
  [a b]
  (let [[ar ac] [(count a) (count (first a))]
        bc (count (first b))]
    (mapv (fn [i]
            (mapv (fn [j]
                    (reduce + (for [k (range ac)]
                                (* (get-in a [i k]) (get-in b [k j])))))
                  (range bc)))
          (range ar))))

(deftest helpers-test
  (testing "addition, subtraction, split, 2x2"
    (is (= [[6 8] [10 12]] (s/matrix-addition [[1 2] [3 4]] [[5 6] [7 8]])))
    (is (= [[-4 -4] [-4 -4]]
           (s/matrix-subtraction [[1 2] [3 4]] [[5 6] [7 8]])))
    (is (= [[[4 3] [2 3]] [[2 4] [1 1]] [[6 5] [8 4]] [[4 3] [1 6]]]
           (s/split-matrix [[4 3 2 4] [2 3 1 1] [6 5 4 3] [8 4 1 6]])))
    (is (= [[19 22] [43 50]]
           (s/default-matrix-multiplication [[1 2] [3 4]] [[5 6] [7 8]])))))

(deftest strassen-test
  (testing "Strassen multiplication"
    (is (= [[34 23 19 15] [68 46 37 28] [28 18 15 12] [96 62 55 48]]
           (s/strassen [[2 1 3] [3 4 6] [1 4 2] [7 6 7]]
                       [[4 2 3 4] [2 1 1 1] [8 6 4 2]])))
    (is (= [[139 163] [121 134] [100 121]]
           (s/strassen [[3 7 5 6 9] [1 5 3 7 8] [1 4 4 5 7]]
                       [[2 4] [5 2] [1 7] [5 5] [7 8]])))
    (testing "matches naive multiplication incl. square inputs"
      (doseq [[a b] [[[[1 2] [3 4]] [[5 6] [7 8]]]
                     [[[1 2 3] [4 5 6]] [[7 8] [9 10] [11 12]]]
                     [[[2 3 4 5] [6 4 3 1] [2 3 6 7] [3 1 2 4]]
                      [[0 2 1 1] [16 2 3 3] [2 2 7 7] [13 11 22 4]]]]]
        (is (= (naive a b) (s/strassen a b)))))
    (testing "dimension mismatch"
      (is (thrown? clojure.lang.ExceptionInfo
                   (s/strassen [[1 2]] [[1 2]]))))))
