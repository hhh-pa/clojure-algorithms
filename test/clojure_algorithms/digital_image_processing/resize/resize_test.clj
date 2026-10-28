(ns clojure-algorithms.digital-image-processing.resize.resize-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.resize.resize
             :refer [make-resizer get-x get-y process]]))

(deftest test-resize
  (let [img [[[1 2 3] [4 5 6]] [[7 8 9] [10 11 12]]]
        nn (make-resizer img 100 100)]
    (is (= 2 (get-x (assoc nn :ratio-x 0.5) 4)))
    (is (= 2 (get-y (assoc nn :ratio-y 0.5) 4))))
  (let [img [[1 2] [3 4]]
        out (process (make-resizer img 4 4))]
    (is (= [[1 1 2 2] [1 1 2 2] [3 3 4 4] [3 3 4 4]] out)))
  (let [img [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]
        out (process (make-resizer img 2 2))]
    (is (= [[1 3] [9 11]] out)))
  (let [img [[[1 2 3]]]]
    (is (= [[[1 2 3]]] (process (make-resizer img 1 1)))))
  (is (thrown? clojure.lang.ExceptionInfo (make-resizer [[1]] -1 5)))
  (is (thrown? clojure.lang.ExceptionInfo (make-resizer [[1]] 5 -1))))
