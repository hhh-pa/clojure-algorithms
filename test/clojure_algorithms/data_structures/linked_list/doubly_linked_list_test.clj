(ns clojure-algorithms.data-structures.linked-list.doubly-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.doubly-linked-list
             :refer [make-list list-empty? ll-str insert-at-nth insert-at-head
                     insert-at-tail delete-at-nth delete-head delete-tail
                     delete-value]]))

(deftest test-doubly
  (let [lst (-> (make-list) (insert-at-head "b") (insert-at-head "a")
                (insert-at-tail "c"))]
    (is (= ["a" "b" "c"] lst)))
  (let [lst (-> (make-list) (insert-at-tail "a") (insert-at-tail "b")
                (insert-at-tail "c"))]
    (is (= "a->b->c" (ll-str lst))))
  (let [lst (reduce (fn [l i] (insert-at-nth l i (inc i))) (make-list)
                    (range 5))]
    (is (= 5 (count lst))))
  (is (thrown? clojure.lang.ExceptionInfo
               (insert-at-nth (make-list) -1 666)))
  (let [lst (make-list)]
    (is (true? (list-empty? lst)))
    (is (= "" (ll-str lst)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-head lst)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-tail lst))))
  (let [lst (reduce (fn [l i] (insert-at-nth l i (inc i))) (make-list)
                    (range 10))]
    (is (= "1->2->3->4->5->6->7->8->9->10" (ll-str lst)))
    (let [lst (-> lst (insert-at-head 0) (insert-at-tail 11))]
      (is (= "0->1->2->3->4->5->6->7->8->9->10->11" (ll-str lst)))
      (let [[d0 lst] (delete-head lst)
            [d9 lst] (delete-at-nth lst 9)
            [d11 lst] (delete-tail lst)]
        (is (= 0 d0))
        (is (= 10 d9))
        (is (= 11 d11))
        (is (= 9 (count lst)))
        (is (= "1->2->3->4->5->6->7->8->9" (ll-str lst))))))
  (let [lst (reduce insert-at-tail (make-list) [1 2 3])]
    (is (= [2 [1 3]] [(first (delete-value lst 2))
                      (second (delete-value lst 2))]))
    (is (thrown? clojure.lang.ExceptionInfo (delete-value lst 99)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-value (make-list) 1)))))
