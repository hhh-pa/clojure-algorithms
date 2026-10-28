(ns clojure-algorithms.financial.streaming-moving-average-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.streaming-moving-average
             :refer [make-streaming-average sma-update! sma-value]]))

(deftest streaming-examples
  (let [calc (make-streaming-average 3)]
    (is (= 0.0 (sma-value calc)))
    (sma-update! calc 10.0)
    (sma-update! calc 12.0)
    (sma-update! calc 2.0)
    (sma-update! calc 1.0)
    (is (= 5.0 (sma-value calc))))
  (let [calc (make-streaming-average 5)]
    (sma-update! calc 2.0)
    (sma-update! calc 3.0)
    (is (= 2.5 (sma-value calc)))))

(deftest streaming-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Window size must be a positive integer"
                        (make-streaming-average 0)))
  (is (thrown? clojure.lang.ExceptionInfo
               (sma-update! (make-streaming-average 3) "nope"))))
