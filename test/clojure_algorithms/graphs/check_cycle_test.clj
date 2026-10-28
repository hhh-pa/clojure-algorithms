(ns clojure-algorithms.graphs.check-cycle-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.check-cycle
             :refer [check-cycle depth-first-search]]))

(deftest test-check-cycle
  (is (false? (check-cycle {0 [] 1 [0 3] 2 [0 4] 3 [5] 4 [5] 5 []})))
  (is (true? (check-cycle {0 [1 2] 1 [2] 2 [0 3] 3 [3]})))
  (is (false? (first (depth-first-search
                      {0 [] 1 [0 3] 2 [0 4] 3 [5] 4 [5] 5 []} 0 #{} #{}))))
  (is (true? (check-cycle {0 [0]}))))
