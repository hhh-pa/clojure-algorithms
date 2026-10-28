(ns clojure-algorithms.data-structures.queues.circular-queue-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.circular-queue
             :refer [make-circular-queue q-size q-empty? q-first enqueue
                     dequeue]]))

(deftest test-circular-queue
  (let [q (make-circular-queue 5)]
    (is (= 0 (q-size q)))
    (is (true? (q-empty? q)))
    (is (false? (q-first q)))
    (let [q (enqueue q "A")]
      (is (false? (q-empty? q)))
      (is (= "A" (q-first q)))
      (is (= 1 (q-size q)))
      (let [q (-> q (enqueue "B") (enqueue "C") (enqueue "D") (enqueue "E"))]
        (is (= 5 (q-size q)))
        (is (= "A" (q-first q)))
        (is (thrown? clojure.lang.ExceptionInfo (enqueue q "F")))
        (let [[a q] (dequeue q)
              [b q] (dequeue q)]
          (is (= ["A" "B"] [a b]))
          (is (= 3 (q-size q)))
          (is (= "C" (q-first q)))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (dequeue (make-circular-queue 5))))
  (let [q (reduce enqueue (make-circular-queue 2) [1 2])
        [a q] (dequeue q)
        q (enqueue q 3)
        [b q] (dequeue q)
        [c q] (dequeue q)]
    (is (= [1 2 3] [a b c]))
    (is (true? (q-empty? q)))))
