(ns clojure-algorithms.data-structures.linked-list.deque-doubly-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.deque-doubly
             :refer [make-deque deque-empty? first-element last-element
                     add-first add-last remove-first remove-last]]))

(deftest test-deque
  (let [dq (make-deque)]
    (is (true? (deque-empty? dq)))
    (is (thrown? clojure.lang.ExceptionInfo (remove-first dq)))
    (is (thrown? clojure.lang.ExceptionInfo (remove-last dq)))
    (is (thrown? clojure.lang.ExceptionInfo (first-element dq)))
    (is (thrown? clojure.lang.ExceptionInfo (last-element dq))))
  (is (= "A" (first-element (add-first (make-deque) "A"))))
  (is (= "B" (first-element (-> (make-deque) (add-first "A") (add-first "B")))))
  (is (= "A" (last-element (add-last (make-deque) "A"))))
  (is (= "B" (last-element (-> (make-deque) (add-last "A") (add-last "B")))))
  (is (= "AV" (first-element (add-first (make-deque) "AV"))))
  (is (= "B" (last-element (add-last (make-deque) "B"))))
  (let [dq (-> (make-deque) (add-first "A"))
        [e dq] (remove-first dq)]
    (is (= "A" e))
    (is (true? (deque-empty? dq))))
  (let [dq (-> (make-deque) (add-last "A") (add-last "B"))
        [e dq] (remove-last dq)]
    (is (= "B" e))
    (is (= ["A"] dq)))
  (let [dq (-> (make-deque) (add-first 1) (add-last 2) (add-first 0))]
    (is (= 3 (count dq)))
    (is (= 0 (first-element dq)))
    (is (= 2 (last-element dq)))))
