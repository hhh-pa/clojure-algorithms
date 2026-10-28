(ns clojure-algorithms.graphs.scc-kosaraju-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.scc-kosaraju :refer [kosaraju]]))

(deftest test-kosaraju
  (is (= #{#{0 1 2} #{3 4}}
         (set (map set (kosaraju {0 [1] 1 [2] 2 [0 3] 3 [4] 4 [3]} 5)))))
  (is (= #{#{0} #{1} #{2}}
         (set (map set (kosaraju {0 [1] 1 [2] 2 []} 3)))))
  (is (= [[] []] [(kosaraju {} 0) (kosaraju {} 0)])))
