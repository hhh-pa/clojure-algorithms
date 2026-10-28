(ns clojure-algorithms.divide-and-conquer.closest-pair-of-points-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.closest-pair-of-points :as cp]))

(deftest euclidean-distance-sqr-test
  (testing "squared distances"
    (is (= 5 (cp/euclidean-distance-sqr [1 2] [2 4])))
    (is (= 0 (cp/euclidean-distance-sqr [3 3] [3 3])))))

(deftest column-based-sort-test
  (testing "sort by column"
    (is (= [[3 0] [5 1] [4 2]]
           (cp/column-based-sort [[5 1] [4 2] [3 0]] 1)))
    (is (= [[2 3] [3 4] [12 30]]
           (cp/column-based-sort [[12 30] [2 3] [3 4]] 0)))))

(deftest brute-force-test
  (testing "brute force closest pair"
    (is (= 5 (cp/dis-between-closest-pair [[1 2] [2 4] [5 7] [8 9] [11 0]])))))

(deftest closest-pair-test
  (testing "divide and conquer closest pair"
    (let [d (cp/closest-pair-of-points [[2 3] [12 30]])]
      (is (< (Math/abs (- d 28.792360097775937)) 1e-9)))
    (let [pts [[2 3] [12 30] [40 50] [5 1] [12 10] [3 4]]
          d (cp/closest-pair-of-points pts)]
      (is (< (Math/abs (- d (Math/sqrt 2))) 1e-9)))
    (testing "agrees with brute force on a larger set"
      (let [pts (vec (for [x (range 8) y (range 5)] [x (* y y)]))]
        (is (= (cp/dis-between-closest-pair pts)
               (cp/closest-pair-of-points-sqr (cp/column-based-sort pts 0)
                                              (cp/column-based-sort pts 1))))))))
