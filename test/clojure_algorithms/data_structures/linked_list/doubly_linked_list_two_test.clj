(ns clojure-algorithms.data-structures.linked-list.doubly-linked-list-two-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.doubly-linked-list-two
             :refer [make-list list-empty? ll-str list-contains? get-head-data
                     get-tail-data set-head set-tail insert-value
                     insert-before-index insert-after-index insert-at-position
                     delete-value]]))

(deftest test-doubly-two
  (let [lst (make-list)]
    (is (nil? (get-head-data lst)))
    (is (nil? (get-tail-data lst)))
    (is (true? (list-empty? lst))))
  (let [lst (insert-value (make-list) 10)]
    (is (= 10 (get-head-data lst)))
    (is (= 10 (get-tail-data lst)))
    (let [lst (insert-at-position lst 3 20)]
      (is (= 10 (get-head-data lst)))
      (is (= 20 (get-tail-data lst)))
      (let [lst (set-head lst {:data 1000})]
        (is (= 1000 (get-head-data lst)))
        (is (= 20 (get-tail-data lst)))
        (let [lst (set-tail lst {:data 2000})]
          (is (= 1000 (get-head-data lst)))
          (is (= 2000 (get-tail-data lst)))
          (is (= [1000 10 20 2000] lst))
          (is (= "1000 10 20 2000" (ll-str lst)))
          (is (true? (list-contains? lst 10)))
          (is (false? (list-contains? lst 99)))))))
  (let [lst [1 2 3]]
    (is (= [1 9 2 3] (insert-before-index lst 1 9)))
    (is (= [1 2 9 3] (insert-after-index lst 1 9)))
    (is (= [1 3] (delete-value lst 2)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-value lst 99)))))
