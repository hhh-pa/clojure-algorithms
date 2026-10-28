(ns clojure-algorithms.graphs.strongly-connected-components-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.strongly-connected-components
             :refer [topology-sort find-components
                     strongly-connected-components]]))

(def g1 {0 [2 3] 1 [0] 2 [1] 3 [4] 4 []})
(def g2 {0 [1 2 3] 1 [2] 2 [0] 3 [4] 4 [5] 5 [3]})

(deftest test-strongly-connected-components
  (is (= [1 2 4 3 0] (topology-sort g1 0)))
  (is (= [2 1 5 4 3 0] (topology-sort g2 0)))
  (is (= [0 1 2] (find-components {0 [1] 1 [2] 2 [0]} 0)))
  (is (= [[0 1 2] [3] [4]] (strongly-connected-components g1)))
  (is (= [[0 2 1] [3 5 4]] (strongly-connected-components g2))))
