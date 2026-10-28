(ns clojure-algorithms.data-structures.queues.monotonic-queue-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.queues.monotonic-queue
             :refer [max-sliding-window]]))

(deftest test-max-sliding-window
  (is (= [3 3 5 5 6 7]
         (max-sliding-window [1 3 -1 -3 5 3 6 7] 3)))
  (is (= [8] (max-sliding-window [8] 1)))
  (is (= [8 -6 -6 -6 3 6 7 7 1]
         (max-sliding-window [8 -7 -6 -7 -6 3 6 7 0 1] 2)))
  (is (= [-1 -1 -1 -1 -1] (max-sliding-window [-1 -1 -1 -1 -1] 1)))
  (is (= [9] (max-sliding-window [9 8 7 6 5 4 3 2 1] 9)))
  (is (= [3 3 3 2] (max-sliding-window [1 2 3 1 2 0] 3)))
  (is (thrown? clojure.lang.ExceptionInfo (max-sliding-window [1] 2)))
  (is (thrown? clojure.lang.ExceptionInfo (max-sliding-window [] 1)))
  (is (thrown? clojure.lang.ExceptionInfo (max-sliding-window [1] 0))))
