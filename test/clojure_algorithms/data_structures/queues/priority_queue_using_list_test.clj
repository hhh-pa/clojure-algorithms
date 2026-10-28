(ns clojure-algorithms.data-structures.queues.priority-queue-using-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.priority-queue-using-list
             :refer [make-fixed-queue fp-str fp-enqueue fp-dequeue
                     make-element-queue ep-str ep-enqueue ep-dequeue]]))

(deftest test-fixed-priority-queue
  (let [fpq (reduce (fn [q [p d]] (fp-enqueue q p d)) (make-fixed-queue)
                    [[0 10] [1 70] [0 100] [2 1] [2 5] [1 7] [2 4] [1 64]
                     [0 128]])]
    (is (= (str "Priority 0: [10, 100, 128]\nPriority 1: [70, 7, 64]\n"
                "Priority 2: [1, 5, 4]")
           (fp-str fpq)))
    (let [[a fpq] (fp-dequeue fpq) [b fpq] (fp-dequeue fpq)
          [c fpq] (fp-dequeue fpq) [d fpq] (fp-dequeue fpq)
          [e fpq] (fp-dequeue fpq)]
      (is (= [10 100 128 70 7] [a b c d e]))
      (is (= "Priority 0: []\nPriority 1: [64]\nPriority 2: [1, 5, 4]"
             (fp-str fpq)))
      (let [[f fpq] (fp-dequeue fpq) [g fpq] (fp-dequeue fpq)
            [h fpq] (fp-dequeue fpq) [i fpq] (fp-dequeue fpq)]
        (is (= [64 1 5 4] [f g h i]))
        (is (thrown? clojure.lang.ExceptionInfo (fp-dequeue fpq)))
        (is (= "Priority 0: []\nPriority 1: []\nPriority 2: []"
               (fp-str fpq))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (fp-enqueue (make-fixed-queue) 3 10))))

(deftest test-element-priority-queue
  (let [epq (reduce ep-enqueue (make-element-queue)
                    [10 70 4 1 5 7 4 64 128])]
    (is (= "[10, 70, 4, 1, 5, 7, 4, 64, 128]" (ep-str epq)))
    (let [[a epq] (ep-dequeue epq) [b epq] (ep-dequeue epq)
          [c epq] (ep-dequeue epq) [d epq] (ep-dequeue epq)
          [e epq] (ep-dequeue epq) [f epq] (ep-dequeue epq)]
      (is (= [1 4 4 5 7 10] [a b c d e f]))
      (is (= "[70, 64, 128]" (ep-str epq)))
      (let [[g epq] (ep-dequeue epq) [h epq] (ep-dequeue epq)
            [i epq] (ep-dequeue epq)]
        (is (= [64 70 128] [g h i]))
        (is (thrown? clojure.lang.ExceptionInfo (ep-dequeue epq)))
        (is (= "[]" (ep-str epq)))))))
