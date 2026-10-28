(ns clojure-algorithms.data-structures.heap.min-heap-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.heap.min-heap
             :refer [make-node node->str make-min-heap heap-peek heap-remove
                     heap-insert heap-empty? decrease-key heap-value]]))

(deftest test-min-heap
  (let [r (make-node "R" -1) b (make-node "B" 6) a (make-node "A" 3)
        x (make-node "X" 1) e (make-node "E" 4)]
    (is (= "Node(B, 6)" (node->str b)))
    (let [h (make-min-heap [r b a x e])]
      (is (= "R" (:name (heap-peek h))))
      (let [h (decrease-key h b -17)]
        (is (= "Node(B, -17)" (node->str (assoc b :val -17))))
        (is (= -17 (heap-value h "B")))
        (is (= "B" (:name (heap-peek h))))
        (let [[n1 h] (heap-remove h)
              [n2 h] (heap-remove h)]
          (is (= "B" (:name n1)))
          (is (= "R" (:name n2)))
          (is (false? (heap-empty? h)))))))
  (let [h (make-min-heap [])]
    (is (true? (heap-empty? h)))
    (let [h (heap-insert h (make-node "Q" 5))]
      (is (= "Q" (:name (heap-peek h))))))
  (let [h (make-min-heap [(make-node "A" 1)])]
    (is (thrown? clojure.lang.ExceptionInfo
                 (decrease-key h (make-node "A" 1) 5)))))
