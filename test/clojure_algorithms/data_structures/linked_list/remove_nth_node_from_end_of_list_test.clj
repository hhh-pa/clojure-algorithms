(ns clojure-algorithms.data-structures.linked-list.remove-nth-node-from-end-of-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.remove-nth-node-from-end-of-list
             :refer [make-list append-item view-list remove-nth-from-end]]))

(deftest test-remove-nth
  (let [[d lst] (append-item (make-list) 5)]
    (is (= 5 d))
    (let [[d lst] (append-item lst 6)]
      (is (= 6 d))
      (is (= "6->5" (view-list lst)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (remove-nth-from-end (make-list) 3)))
  (let [[_ lst] (reduce (fn [[_ l] v] (append-item l v)) [nil (make-list)]
                        [5 6 8 8 10 12 17])]
    (is (thrown? clojure.lang.ExceptionInfo (remove-nth-from-end lst 100))))
  (let [[_ lst] (reduce (fn [[_ l] v] (append-item l v)) [nil (make-list)]
                        [5 6 8 8 10 12 17 -25 -20])]
    (is (= "-20->-25->17->12->10->8->6->5"
           (view-list (remove-nth-from-end lst 3))))
    (is (= "-25->17->12->10->8->8->6->5"
           (view-list (remove-nth-from-end lst 9))))
    (is (= "-20->-25->17->12->10->8->8->6"
           (view-list (remove-nth-from-end lst 1))))))
