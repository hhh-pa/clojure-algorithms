(ns clojure-algorithms.data-structures.binary-tree.non-recursive-segment-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.non-recursive-segment-tree
             :refer [make-seg-tree seg-update seg-query]]))

(deftest test-non-recursive-seg-tree
  (is (= 6 (seg-query (make-seg-tree [1 2 3] +) 0 2)))
  (is (= 1 (seg-query (make-seg-tree [3 1 2] min) 0 2)))
  (is (= 3 (seg-query (make-seg-tree [2 3 1] max) 0 2)))
  (let [st (-> (make-seg-tree [1 5 7 -1 6] +)
               (seg-update 1 -1) (seg-update 2 3))]
    (is (= 2 (seg-query st 1 2)))
    (is (= -1 (seg-query st 1 1)))
    (let [st (seg-update st 4 1)]
      (is (= 0 (seg-query st 3 4)))))
  (let [st (make-seg-tree [[1 2 3] [3 2 1] [1 1 1]]
                          (fn [a b] (mapv + a b)))]
    (is (= [4 4 4] (seg-query st 0 1)))
    (is (= [4 3 2] (seg-query st 1 2)))
    (let [st (seg-update st 1 [-1 -1 -1])]
      (is (= [0 0 0] (seg-query st 1 2)))
      (is (= [1 2 3] (seg-query st 0 2)))))
  (is (= "abc" (seg-query (make-seg-tree ["a" "b" "c"] str) 0 2)))
  (is (= [6 9] (seg-query (make-seg-tree [[1 2] [2 3] [3 4]]
                                         (fn [a b] [(+ (nth a 0) (nth b 0))
                                                    (+ (nth a 1) (nth b 1))]))
                          0 2)))
  (is (= 1 (seg-query (make-seg-tree [3 1 2 4] min) 0 3))))

(deftest test-all-segments
  (let [arr [1 10 -2 9 -3 8 4 -7 5 6 11 -12]
        n (count arr)]
    (doseq [[f red] [[min min] [max max] [+ +]]]
      (let [st (make-seg-tree arr f)]
        (doseq [i (range n) j (range i n)]
          (is (= (reduce red (subvec (vec arr) i (inc j)))
                 (seg-query st i j))))))))
