(ns clojure-algorithms.graphs.bellman-ford-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.bellman-ford :refer [bellman-ford]]))

(deftest test-bellman-ford
  (let [g (mapv (fn [[s d w]] {:src s :dst d :weight w})
                [[2 1 -10] [3 2 3] [0 3 5] [0 1 4]])]
    (is (= [0.0 -2.0 8.0 5.0] (bellman-ford g 4 4 0))))
  (let [g (mapv (fn [[s d w]] {:src s :dst d :weight w})
                [[2 1 -10] [3 2 3] [0 3 5] [0 1 4] [1 3 5]])]
    (is (thrown? Exception (bellman-ford g 4 5 0)))))
