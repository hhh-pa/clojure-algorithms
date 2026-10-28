(ns clojure-algorithms.data-structures.linked-list.circular-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.circular-linked-list
             :refer [make-list list-empty? ll-str insert-nth insert-head
                     insert-tail delete-nth delete-front delete-tail]]))

(deftest test-circular
  (let [lst (make-list)]
    (is (= 0 (count lst)))
    (is (true? (list-empty? lst)))
    (is (= "" (ll-str lst)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-front lst)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-tail lst)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-nth lst -1)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-nth lst 0))))
  (let [lst (reduce (fn [l i] (insert-nth l i (inc i))) (make-list) (range 5))]
    (is (= "1->2->3->4->5" (ll-str lst)))
    (let [lst (insert-tail lst 6)]
      (is (= "1->2->3->4->5->6" (ll-str lst)))
      (let [lst (insert-head lst 0)]
        (is (= "0->1->2->3->4->5->6" (ll-str lst)))
        (let [[d0 lst] (delete-front lst)
              [d6 lst] (delete-tail lst)]
          (is (= 0 d0))
          (is (= 6 d6))
          (is (= "1->2->3->4->5" (ll-str lst)))
          (let [[d2 lst] (delete-nth lst 2)]
            (is (= 3 d2))
            (is (= "1->2->3->4->5" (ll-str (insert-nth lst 2 3))))
            (is (false? (list-empty? lst)))))))))
