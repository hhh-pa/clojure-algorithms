(ns clojure-algorithms.data-structures.queues.queue-by-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.queue-by-list
             :refer [make-queue queue-repr put queue-get queue-rotate
                     get-front]]))

(deftest test-queue-by-list
  (is (= "Queue(())" (queue-repr (make-queue))))
  (is (= "Queue((10, 20, 30))" (queue-repr (make-queue [10 20 30]))))
  (is (= "Queue((1, 4, 9))" (queue-repr (make-queue (map #(* % %) [1 2 3])))))
  (is (= 0 (count (make-queue))))
  (is (= 26 (count (make-queue (map str "abcdefghijklmnopqrstuvwxyz")))))
  (let [q (reduce put (make-queue) (range 1 11))]
    (is (= 10 (count q)))
    (let [[a q] (queue-get q) [b q] (queue-get q)]
      (is (= [1 2] [a b]))
      (is (= 8 (count q)))))
  (let [q (-> (make-queue) (put 10))]
    (is (= "Queue((10,))" (queue-repr q)))
    (let [q (-> q (put 20) (put 30))]
      (is (= "Queue((10, 20, 30))" (queue-repr q)))))
  (let [q (make-queue [10 20 30])
        [a q] (queue-get q)]
    (is (= 10 a))
    (let [q (put q 40)
          [b q] (queue-get q)
          [c q] (queue-get q)]
      (is (= [20 30] [b c]))
      (is (= 1 (count q)))
      (let [[d q] (queue-get q)]
        (is (= 40 d))
        (is (thrown? clojure.lang.ExceptionInfo (queue-get q))))))
  (let [q (make-queue [10 20 30 40])]
    (is (= "Queue((20, 30, 40, 10))" (queue-repr (queue-rotate q 1))))
    (is (= "Queue((40, 10, 20, 30))" (queue-repr (queue-rotate q 3)))))
  (let [q (make-queue [10 20 30])]
    (is (= 10 (get-front q)))
    (let [[_ q] (queue-get q)]
      (is (= 20 (get-front q)))))
  (is (thrown? clojure.lang.ExceptionInfo (get-front (make-queue)))))
