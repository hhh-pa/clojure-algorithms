(ns clojure-algorithms.financial.exponential-moving-average-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.exponential-moving-average
             :refer [exponential-moving-average]]))

(deftest ema-examples
  (is (= [2.0 3.5 3.25 5.725 5.8625 7.43125 8.715625]
         (vec (exponential-moving-average [2 5 3 8.2 6 9 10] 3))))
  (is (= [10.1 20.02 30.003]
         (vec (exponential-moving-average [10.1 20.02 30.003] 1))))
  (is (= [] (vec (exponential-moving-average [] 3))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"window_size must be > 0"
                        (exponential-moving-average [1 2] 0))))
