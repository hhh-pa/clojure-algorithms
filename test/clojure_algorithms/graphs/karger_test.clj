(ns clojure-algorithms.graphs.karger-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.karger :refer [partition-graph]]))

(deftest test-karger
  (is (= #{["0" "1"]} (partition-graph {"0" ["1"] "1" ["0"]})))
  (let [g {"1" ["2" "3" "4" "5"] "2" ["1" "3" "4" "5"]
           "3" ["1" "2" "4" "5" "10"] "4" ["1" "2" "3" "5" "6"]
           "5" ["1" "2" "3" "4" "7"] "6" ["7" "8" "9" "10" "4"]
           "7" ["6" "8" "9" "10" "5"] "8" ["6" "7" "9" "10"]
           "9" ["6" "7" "8" "10"] "10" ["6" "7" "8" "9" "3"]}
        cut (partition-graph g)]
    (is (seq cut))
    (doseq [[a b] cut]
      (is (some #{b} (get g a))))))
