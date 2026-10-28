(ns clojure-algorithms.data-structures.binary-tree.fenwick-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.fenwick-tree
             :refer [make-fenwick make-fenwick-sized next-index prev-index
                     fenwick-add fenwick-prefix fenwick-query fenwick-get
                     fenwick-update fenwick-array rank-query]]))

(deftest test-fenwick-build
  (let [a [1 2 3 4 5]
        f1 (make-fenwick a)
        f2 (reduce (fn [ft [i v]] (fenwick-add ft i v))
                   (make-fenwick-sized (count a)) (map-indexed vector a))]
    (is (= (:tree f1) (:tree f2))))
  (let [a (vec (range 128))]
    (is (= a (fenwick-array (make-fenwick a)))))
  (is (= [2 4 6 8 10]
         (fenwick-array (reduce (fn [ft [i v]] (fenwick-add ft i v))
                                (make-fenwick [1 2 3 4 5])
                                (map-indexed vector [1 2 3 4 5])))))
  (is (= [1 2 3 4 5]
         (fenwick-array (reduce (fn [ft [i v]] (fenwick-update ft i v))
                                (make-fenwick [5 4 3 2 1])
                                (map-indexed vector [1 2 3 4 5]))))))

(deftest test-fenwick-queries
  (let [a [3 1 4 1 5 9 2 6]
        f (make-fenwick a)]
    (doseq [i (range (inc (count a)))]
      (is (= (reduce + 0 (take i a)) (fenwick-prefix f i))))
    (doseq [i (range (count a)) j (range (inc i) (inc (count a)))]
      (is (= (reduce + 0 (subvec (vec a) i j)) (fenwick-query f i j))))
    (doseq [i (range (count a))]
      (is (= (nth a i) (fenwick-get f i)))))
  (is (= 0 (next-index 0)))
  (is (= 4 (next-index 2)))
  (is (= 0 (prev-index 1)))
  (is (= 2 (prev-index 3))))

(deftest test-rank-query
  (let [f (make-fenwick [1 2 0 3 0 5])]
    (is (= -1 (rank-query f 0)))
    (is (= 0 (rank-query f 2)))
    (is (= 0 (rank-query f 1)))
    (is (= 2 (rank-query f 3)))
    (is (= 2 (rank-query f 5)))
    (is (= 4 (rank-query f 6)))
    (is (= 5 (rank-query f 11)))))
