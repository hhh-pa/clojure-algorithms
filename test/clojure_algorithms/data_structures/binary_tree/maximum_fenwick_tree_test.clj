(ns clojure-algorithms.data-structures.binary-tree.maximum-fenwick-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.maximum-fenwick-tree
             :refer [make-max-fenwick max-update max-query]]))

(deftest test-max-fenwick
  (let [ft (make-max-fenwick 5)]
    (is (= 0 (max-query ft 0 5)))
    (let [ft (max-update ft 4 100)]
      (is (= 100 (max-query ft 0 5)))
      (let [ft (-> ft (max-update 4 0) (max-update 2 20))]
        (is (= 20 (max-query ft 0 5)))
        (let [ft (max-update ft 4 10)]
          (is (= 20 (max-query ft 2 5)))
          (is (= 20 (max-query ft 1 5)))
          (let [ft (max-update ft 2 0)]
            (is (= 10 (max-query ft 0 5))))))))
  (let [ft (max-update (make-max-fenwick 10000) 255 30)]
    (is (= 30 (max-query ft 0 10000))))
  (is (= 1 (max-query (max-update (make-max-fenwick 6) 5 1) 5 6)))
  (is (= 1000 (max-query (max-update (make-max-fenwick 6) 0 1000) 0 1)))
  (let [ft (-> (make-max-fenwick 8) (max-update 4 20) (max-update 5 1))]
    (is (= 20 (max-query ft 0 6)))
    (is (= 1 (max-query (max-update ft 4 0) 0 6)))))

(deftest test-max-fenwick-randomized
  (let [n 50
        vals (vec (map #(mod (* % 37) 100) (range n)))
        ft (reduce (fn [t [i v]] (max-update t i v))
                   (make-max-fenwick n) (map-indexed vector vals))]
    (doseq [l (range n) r (range (inc l) (inc n))]
      (is (= (apply max (subvec vals l r)) (max-query ft l r))))))
