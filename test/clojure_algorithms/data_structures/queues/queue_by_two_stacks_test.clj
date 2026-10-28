(ns clojure-algorithms.data-structures.queues.queue-by-two-stacks-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.queue-by-two-stacks
             :refer [make-queue queue-len queue-repr put queue-get]]))

(deftest test-queue-by-two-stacks
  (is (= "Queue(())" (queue-repr (make-queue))))
  (is (= 0 (queue-len (make-queue))))
  (let [q (make-queue [1 2 3])]
    (is (= 3 (queue-len q)))
    (let [[a q] (queue-get q)
          [b q] (queue-get q)
          q (put q 4)
          [c q] (queue-get q)
          [d q] (queue-get q)]
      (is (= [1 2 3 4] [a b c d]))
      (is (= 0 (queue-len q)))
      (is (thrown? clojure.lang.ExceptionInfo (queue-get q)))))
  (let [q (-> (make-queue) (put 10) (put 20))]
    (is (= "Queue((10, 20))" (queue-repr q)))
    (let [[_ q] (queue-get q)]
      (is (= "Queue((20,))" (queue-repr q))))))
