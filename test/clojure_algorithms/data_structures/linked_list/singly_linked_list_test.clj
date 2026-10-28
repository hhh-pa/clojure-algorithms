(ns clojure-algorithms.data-structures.linked-list.singly-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.singly-linked-list
             :refer [make-node node->str make-list list-empty? ll-str ll-nth
                     ll-assoc insert-nth insert-head insert-tail delete-nth
                     delete-head delete-tail ll-reverse]]))

(deftest test-node
  (is (= "Node(20)" (node->str (make-node 20))))
  (is (= "Node(Hello, world!)" (node->str (make-node "Hello, world!"))))
  (is (= "Node()" (node->str (make-node nil))))
  (is (= "Node(true)" (node->str (make-node true)))))

(deftest test-singly-ops
  (let [lst (make-list)]
    (is (true? (list-empty? lst)))
    (is (= "" (ll-str lst)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-head lst)))
    (is (thrown? clojure.lang.ExceptionInfo (delete-tail lst))))
  (let [lst (reduce (fn [l i] (insert-nth l i (inc i))) (make-list) (range 10))]
    (is (= "1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10" (ll-str lst)))
    (let [lst (-> lst (insert-head 0) (insert-tail 11))]
      (is (= "0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10 -> 11"
             (ll-str lst)))
      (let [[d0 lst] (delete-head lst)
            [d9 lst] (delete-nth lst 9)
            [d11 lst] (delete-tail lst)]
        (is (= 0 d0))
        (is (= 10 d9))
        (is (= 11 d11))
        (is (= 9 (count lst)))
        (is (= "1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9" (ll-str lst)))
        (is (every? true? (map-indexed (fn [i _] (= (ll-nth lst i) (inc i)))
                                       (range 9))))
        (let [lst (reduce (fn [l i] (ll-assoc l i (- i))) lst (range 9))]
          (is (every? true? (map-indexed (fn [i _] (= (ll-nth lst i) (- i)))
                                         (range 9))))
          (is (= "-8 -> -7 -> -6 -> -5 -> -4 -> -3 -> -2 -> -1 -> 0"
                 (ll-str (ll-reverse lst)))))))))

(deftest test-mixed-data
  (let [lst (reduce insert-tail (make-list)
                    [-9 100 (make-node 77345112) "dlrow olleH" 7 5555 0
                     -192.55555 "Hello, world!" 77.9 (make-node 10) nil nil
                     12.2])]
    (is (false? (list-empty? lst)))
    (is (= (str "-9 -> 100 -> Node(77345112) -> dlrow olleH -> 7 -> 5555 -> "
                "0 -> -192.55555 -> Hello, world! -> 77.9 -> Node(10) ->  -> "
                " -> 12.2")
           (ll-str lst)))
    (let [[d lst] (delete-head lst)]
      (is (= -9 d))
      (let [[d lst] (delete-tail lst)]
        (is (= 12.2 d))
        (let [[d lst] (delete-nth lst 10)]
          (is (nil? d))
          (let [lst (-> lst
                        (insert-head (make-node "Hello again, world!"))
                        (insert-tail nil)
                        (ll-reverse))]
            (is (= (str " ->  -> Node(10) -> 77.9 -> Hello, world! -> "
                        "-192.55555 -> 0 -> 5555 -> 7 -> dlrow olleH -> "
                        "Node(77345112) -> 100 -> Node(Hello again, world!)")
                   (ll-str lst)))))))))
