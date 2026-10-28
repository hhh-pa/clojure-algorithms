(ns clojure-algorithms.data-structures.queues.linked-queue-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.linked-queue
             :refer [make-queue q-empty? q-str put queue-get queue-clear]]))

(deftest test-linked-queue
  (let [q (make-queue)]
    (is (true? (q-empty? q))))
  (let [q (-> (make-queue) (put "5") (put "9") (put "python"))]
    (is (= "5 <- 9 <- python" (q-str q)))
    (is (= 3 (count q)))
    (let [[a q] (queue-get q)]
      (is (= "5" a))
      (let [[b q] (queue-get q)]
        (is (= "9" b))
        (is (= "python" (q-str q)))
        (is (= 1 (count q))))))
  (let [q (queue-clear (put (make-queue) 1))]
    (is (true? (q-empty? q))))
  (is (thrown? clojure.lang.ExceptionInfo (queue-get (make-queue)))))
