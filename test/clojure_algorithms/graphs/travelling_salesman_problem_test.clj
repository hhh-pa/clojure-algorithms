(ns clojure-algorithms.graphs.travelling-salesman-problem-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.travelling-salesman-problem
             :refer [euclidean-distance validate-graph
                     travelling-salesman-brute-force
                     travelling-salesman-dynamic-programming]]))

(deftest test-tsp
  (is (= 5.0 (euclidean-distance [0 0] [3 4])))
  (is (= 0.0 (euclidean-distance [1 1] [1 1])))
  (is (thrown? IllegalArgumentException (euclidean-distance [1 1] ["a" 1])))
  (is (thrown? clojure.lang.ExceptionInfo (validate-graph [10 20])))
  (let [g (array-map "A" [10 20] "B" [30 21] "C" [15 35])
        [bp bd] (travelling-salesman-brute-force g)
        [dp dd] (travelling-salesman-dynamic-programming g)]
    (is (contains? #{["A" "B" "C" "A"] ["A" "C" "B" "A"]} bp))
    (is (< (Math/abs (- bd 56.3546572)) 1e-6))
    (is (= ["A" "C" "B" "A"] dp))
    (is (< (Math/abs (- dd 56.35465722402587)) 1e-9))))
