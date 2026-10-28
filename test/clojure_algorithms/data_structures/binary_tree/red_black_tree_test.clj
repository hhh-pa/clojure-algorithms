(ns clojure-algorithms.data-structures.binary-tree.red-black-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.red-black-tree
             :refer [make-node rb-insert rb-remove rb-contains? rb-search
                     rb-floor rb-ceil rb-max rb-min rb-count inorder-labels
                     preorder-labels postorder-labels rotate-left-node
                     rotate-right-node check-color-properties? black-height]]))

(defn- build [& labels]
  (reduce rb-insert nil labels))

(deftest test-rotations
  (let [tree (make-node 0 :black
                        (make-node -10 :black (make-node -20) (make-node -5))
                        (make-node 10 :black (make-node 5) (make-node 20)))
        left-rot (make-node 10 :black
                            (make-node 0 :black
                                       (make-node -10 :black (make-node -20)
                                                  (make-node -5))
                                       (make-node 5))
                            (make-node 20))]
    (is (= left-rot (rotate-left-node tree))))
  (let [tree (make-node 0 :black
                        (make-node -10 :black (make-node -20) (make-node -5))
                        (make-node 10 :black (make-node 5) (make-node 20)))
        right-rot (make-node -10 :black
                             (make-node -20)
                             (make-node 0 :black (make-node -5)
                                        (make-node 10 :black (make-node 5)
                                                   (make-node 20))))]
    (is (= right-rot (-> tree rotate-left-node rotate-right-node
                         rotate-right-node)))))

(deftest test-insert-search
  (let [tree (build 0 8 -8 4 12 10 11)]
    (is (= [-8 0 4 8 10 11 12] (vec (inorder-labels tree))))
    (is (true? (check-color-properties? tree)))
    (doseq [x [11 12 -8 0]]
      (is (true? (rb-contains? tree x))))
    (doseq [x [5 -6 -10 13]]
      (is (false? (rb-contains? tree x))))
    (is (= 11 (:label (rb-search tree 11))))
    (is (nil? (rb-search tree 13)))
    (is (= 7 (rb-count tree)))
    (is (= :black (:color tree)))))

(deftest test-insert-delete
  (let [tree (-> (build 0 -12 8 -8 15 4 12 10 9 11)
                 (rb-remove 15) (rb-remove -12) (rb-remove 9))]
    (is (true? (check-color-properties? tree)))
    (is (= [-8 0 4 8 10 11 12] (vec (inorder-labels tree))))))

(deftest test-floor-ceil-min-max
  (let [tree (build 0 -16 16 8 24 20 22)]
    (doseq [[v f c] [[-20 nil -16] [-10 -16 0] [8 8 8] [50 24 nil]]]
      (is (= f (rb-floor tree v)))
      (is (= c (rb-ceil tree v))))
    (is (= 24 (rb-max tree)))
    (is (= -16 (rb-min tree))))
  (is (nil? (rb-max nil)))
  (is (nil? (rb-min nil))))

(deftest test-traversals
  (let [tree (build 0 -16 16 8 24 20 22)]
    (is (= [-16 0 8 16 20 22 24] (vec (inorder-labels tree))))
    (is (= (sort (inorder-labels tree))
           (sort (preorder-labels tree)) (sort (postorder-labels tree))))
    (is (= 7 (count (preorder-labels tree)) (count (postorder-labels tree))))
    (is (some? (black-height tree)))))

(deftest test-balance-stress
  (let [tree (reduce rb-insert nil (range 1000))]
    (is (= (range 1000) (inorder-labels tree)))
    (is (true? (check-color-properties? tree)))))
