(ns clojure-algorithms.data-structures.linked-list.kth-element-from-end-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.kth-element-from-end
             :refer [make-list ll-str add-item kth-element-from-end]]))

(deftest test-kth-from-end
  (let [lst (-> (make-list) (add-item 23) (add-item 14) (add-item 9))]
    (is (= "9 --> 14 --> 23" (ll-str lst)))
    (is (= 23 (kth-element-from-end lst 1)))
    (is (= 14 (kth-element-from-end lst 2)))
    (is (= 9 (kth-element-from-end lst 3)))
    (is (nil? (kth-element-from-end lst 0)))
    (is (nil? (kth-element-from-end lst 4))))
  (is (= 0 (count (make-list))))
  (let [lst (-> (make-list) (add-item "a") (add-item "b"))]
    (is (= 2 (count lst))))
  (is (nil? (kth-element-from-end (make-list) 1)))
  (is (thrown? clojure.lang.ExceptionInfo (add-item (make-list) 1 -1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (add-item (add-item (make-list) 1) 2 5)))
  (let [lst (-> (make-list) (add-item 1) (add-item 2) (add-item 3))]
    (is (= "3 --> 2 --> 1" (ll-str lst)))))
