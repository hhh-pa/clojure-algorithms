(ns clojure-algorithms.graphs.frequent-pattern-graph-miner-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.frequent-pattern-graph-miner
             :refer [get-distinct-edge get-bitcode get-frequency-table
                     get-nodes get-cluster get-support preprocess]]))

(def edge-array
  [["ab-e1" "ac-e3" "bd-e2"] ["ab-e1" "bc-e4"]
   ["ac-e3" "bd-e2" "bc-e4" "cd-e2"]])

(deftest test-frequent-pattern-miner
  (is (= ["a" "b" "c"] (sort (get-distinct-edge edge-array))))
  (is (= "111" (get-bitcode edge-array "a")))
  (is (= "001" (get-bitcode edge-array "c")))
  (is (= {"11111" ["ab" "ac" "df" "bd" "bc"]}
         (get-nodes [["ab" 5 "11111"] ["ac" 5 "11111"] ["df" 5 "11111"]
                     ["bd" 5 "11111"] ["bc" 5 "11111"]])))
  (is (= [100.0 80.0 60.0 40.0 20.0]
         (get-support (array-map 5 {"11111" ["ab"]} 4 {"11101" ["ef"]}
                                 3 {"11001" ["ad"]} 2 {"10010" ["dh"]}
                                 1 {"00100" ["fh"]}))))
  (is (= [[["ab" "e1"] ["ac" "e3"]]] (preprocess [["ab-e1" "ac-e3"]]))))
