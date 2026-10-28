(ns clojure-algorithms.data-structures.queues.double-ended-queue-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.double-ended-queue
             :refer [make-deque deque-empty? deque-str deque-count deque-append
                     deque-appendleft deque-extend deque-extendleft deque-pop
                     deque-popleft deque-clear deque-copy deque-eq?
                     deque-reversed]]))

(deftest test-deque
  (is (= "[]" (deque-str (make-deque []))))
  (is (= "[1, 2, 3]" (deque-str (make-deque [1 2 3]))))
  (is (= "['a', 'b']" (deque-str (make-deque ["a" "b"]))))
  (is (false? (deque-eq? (make-deque [1 2 3]) [1 3 2])))
  (is (true? (deque-eq? (make-deque [1 2 3]) [1 2 3])))
  (is (= 3 (deque-count (make-deque [1 2 3]))))
  (is (true? (deque-empty? (make-deque []))))
  (is (false? (deque-empty? (make-deque [1]))))
  (let [[v dq] (deque-pop (make-deque [1 2 3]))]
    (is (= 3 v))
    (is (= [1 2] dq)))
  (let [[v dq] (deque-popleft (make-deque [1 2 3]))]
    (is (= 1 v))
    (is (= [2 3] dq)))
  (is (thrown? clojure.lang.ExceptionInfo (deque-pop (make-deque []))))
  (is (thrown? clojure.lang.ExceptionInfo (deque-popleft (make-deque []))))
  (is (= [1 2 3 4] (deque-append (make-deque [1 2 3]) 4)))
  (is (= [0 1 2 3] (deque-appendleft (make-deque [1 2 3]) 0)))
  (is (= [1 2 3 4 5] (deque-extend (make-deque [1 2 3]) [4 5])))
  (is (= [-1 0 1 2 3]
         (deque-extendleft (make-deque [1 2 3]) [0 -1])))
  (is (= [] (deque-clear (make-deque [1 2 3]))))
  (is (= [1 2 3] (deque-copy (make-deque [1 2 3]))))
  (is (= [3 2 1] (deque-reversed (make-deque [1 2 3])))))
