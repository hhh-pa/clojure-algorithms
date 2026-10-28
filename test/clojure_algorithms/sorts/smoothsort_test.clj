(ns clojure-algorithms.sorts.smoothsort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.smoothsort :refer [smoothsort sift trinkle]]))

(deftest test-sift-trinkle
  (is (= [3 4 5] (sift [3 5 4] 2 2)))
  (is (= [1 2 3] (sift [1 2 3] 2 2)))
  (is (= [7] (sift [7] 0 1)))
  (is (= [3 1 9 5 8] (sift [9 1 8 5 3] 4 3)))
  (is (= [1 3 5] (trinkle [1 5 3] 2 [1 1] 1)))
  (is (= [3 4 5] (trinkle [3 5 4] 2 [2] 0))))

(deftest test-smoothsort
  (testing "doctest examples"
    (is (= [1 3 4 7 9] (smoothsort [4 1 3 9 7])))
    (is (= [] (smoothsort [])))
    (is (= [1] (smoothsort [1])))
    (is (= [1 2 3 4 5] (smoothsort [5 4 3 2 1])))
    (is (= [1 2 2 3 3] (smoothsort [3 3 2 1 2])))
    (is (= ["a" "b" "c" "d"] (smoothsort ["d" "a" "c" "b"])))
    (is (= [-1 0.0 2.5] (smoothsort [2.5 -1 0.0])))
    (is (= [1 2 3 4 5] (smoothsort [1 2 3 4 5])))
    (is (= [-3 -1 0 2 5] (smoothsort [-3 0 -1 5 2]))))
  (testing "random trials"
    (doseq [_ (range 30)]
      (let [data (vec (repeatedly (rand-int 30) #(- (rand-int 100) 50)))]
        (is (= (vec (sort data)) (smoothsort data)) (str data))))))
