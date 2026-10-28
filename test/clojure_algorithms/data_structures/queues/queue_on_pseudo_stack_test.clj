(ns clojure-algorithms.data-structures.queues.queue-on-pseudo-stack-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.queue-on-pseudo-stack
             :refer [make-queue queue-size queue-str put rotate queue-get
                     front]]))

(deftest test-pseudo-stack-queue
  (let [q (make-queue)]
    (is (= 0 (queue-size q)))
    (is (= "<>" (queue-str q))))
  (let [q (reduce put (make-queue) [1 2 3])]
    (is (= 3 (queue-size q)))
    (let [[a q] (queue-get q)
          [b q] (queue-get q)
          [c q] (queue-get q)]
      (is (= [1 2 3] [a b c]))
      (is (= 0 (queue-size q)))))
  (let [q (-> (make-queue) (put 2) (put 4))
        [f q] (front q)]
    (is (= 2 f))
    (is (= 2 (queue-size q)))
    ;; front re-queues the peeked item at the back (as in Python)
    (let [[a _] (queue-get q)]
      (is (= 4 a))))
  (let [q (reduce put (make-queue) [10 20])]
    (is (= "<10, 20>" (queue-str q)))
    (is (= [20 10] (:stack (rotate q 1))))))
