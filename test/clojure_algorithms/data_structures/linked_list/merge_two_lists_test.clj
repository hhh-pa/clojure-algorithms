(ns clojure-algorithms.data-structures.linked-list.merge-two-lists-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.merge-two-lists
             :refer [test-data-odd test-data-even make-sorted-list ll-str
                     merge-lists]]))

(deftest test-merge-two-lists
  (is (= (sort test-data-odd) (seq (make-sorted-list test-data-odd))))
  (is (= (sort test-data-even) (seq (make-sorted-list test-data-even))))
  (is (every? true? (map-indexed (fn [i _] (= (count (make-sorted-list (range i))) i))
                                 (range 3))))
  (is (= 8 (count (make-sorted-list test-data-odd))))
  (is (= "" (ll-str (make-sorted-list []))))
  (is (= "-11 -> -1 -> 0 -> 1 -> 3 -> 5 -> 7 -> 9"
         (ll-str (make-sorted-list test-data-odd))))
  (is (= "-2 -> 0 -> 2 -> 3 -> 4 -> 6 -> 8 -> 10"
         (ll-str (make-sorted-list test-data-even))))
  (let [merged (merge-lists (make-sorted-list test-data-odd)
                            (make-sorted-list test-data-even))]
    (is (= 16 (count merged)))
    (is (= [-11 -2 -1 0 0 1 2 3 3 4 5 6 7 8 9 10] merged))))
