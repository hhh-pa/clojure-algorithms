(ns clojure-algorithms.data-structures.linked-list.print-reverse-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.print-reverse
             :refer [make-list ll-str append-item extend-list make-linked-list
                     in-reverse]]))

(deftest test-print-reverse
  (let [lst (make-list)]
    (is (= lst []))
    (is (= "" (ll-str lst)))
    (let [lst (append-item lst 1)]
      (is (= "1" (ll-str lst)))
      (let [lst (extend-list lst [2 3 4 5])]
        (is (= "1 -> 2 -> 3 -> 4 -> 5" (ll-str lst))))))
  (is (= "" (ll-str (extend-list (make-list) []))))
  (is (= "1 -> 2" (ll-str (extend-list (make-list) [1 2]))))
  (is (thrown? clojure.lang.ExceptionInfo (make-linked-list [])))
  (is (= "7" (ll-str (make-linked-list [7]))))
  (is (= "abc" (ll-str (make-linked-list ["abc"]))))
  (is (= "7 -> 25" (ll-str (make-linked-list [7 25]))))
  (is (= "" (in-reverse (make-list))))
  (is (= "73 <- 88 <- 69" (in-reverse (make-linked-list [69 88 73]))))
  (let [lst (make-linked-list [14 52 14 12 43])]
    (is (= "14 -> 52 -> 14 -> 12 -> 43" (ll-str lst)))
    (is (= "43 <- 12 <- 14 <- 52 <- 14" (in-reverse lst)))))
