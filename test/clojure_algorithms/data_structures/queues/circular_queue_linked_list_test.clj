(ns clojure-algorithms.data-structures.queues.circular-queue-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.circular-queue-linked-list
             :refer [make-circular-queue q-empty? q-str enqueue dequeue
                     q-first]]))

(deftest test-circular-ll-queue
  (let [q (-> (make-circular-queue 2) (enqueue "a") (enqueue "b"))]
    (is (= "a b" (q-str q)))
    (is (thrown? clojure.lang.ExceptionInfo (enqueue q "c")))
    (let [[a q] (dequeue q)]
      (is (= "a" a))
      (let [[b q] (dequeue q)]
        (is (= "b" b))
        (is (true? (q-empty? q)))
        (is (thrown? clojure.lang.ExceptionInfo (dequeue q))))))
  (let [q (make-circular-queue)]
    (is (= "" (q-str q)))
    (is (true? (q-empty? q)))
    (let [q (-> q (enqueue "a") (enqueue "b"))]
      (is (= "a b" (q-str q)))
      (let [[a q] (dequeue q)]
        (is (= "a" a))
        (is (= "b" (q-first q)))
        (is (= "b" (q-str q))))))
  (let [q (make-circular-queue)]
    (is (thrown? clojure.lang.ExceptionInfo (q-first q))))
  (let [q (-> (make-circular-queue) (enqueue "a"))]
    (is (= "a" (q-first q)))
    (is (= "a" (q-first q)))))
