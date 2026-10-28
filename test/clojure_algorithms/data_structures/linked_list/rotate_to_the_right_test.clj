(ns clojure-algorithms.data-structures.linked-list.rotate-to-the-right-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.rotate-to-the-right
             :refer [insert-node print-linked-list ll-str rotate-to-the-right]]))

(deftest test-rotate-right
  (let [head (-> nil (insert-node 0) (insert-node 2) (insert-node 1))]
    (is (= "0->2->1" (ll-str head)))
    (let [head (-> head (insert-node 4) (insert-node 5))]
      (is (= "0->2->1->4->5" (ll-str head)))))
  (let [head (-> nil (insert-node 10) (insert-node 9) (insert-node 8))]
    (is (= "10->9->8" (ll-str head))))
  (is (thrown? clojure.lang.ExceptionInfo (rotate-to-the-right nil 1)))
  (is (thrown? clojure.lang.ExceptionInfo (rotate-to-the-right [] 1)))
  (let [head (insert-node nil 1)]
    (is (= head (rotate-to-the-right head 1))))
  (let [head (reduce insert-node nil [1 2 3 4 5])]
    (is (= "4->5->1->2->3" (ll-str (rotate-to-the-right head 2)))))
  (let [head (reduce insert-node nil [5 1 2 4 3])]
    (is (= "2->4->3->5->1" (ll-str (rotate-to-the-right head 3)))))
  (print-linked-list [1 2]))
