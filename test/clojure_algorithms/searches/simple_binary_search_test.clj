(ns clojure-algorithms.searches.simple-binary-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.simple-binary-search :as sbs]))

(deftest binary-search-test
  (testing "boolean binary search"
    (let [test-list [0 1 2 8 13 17 19 32 42]]
      (is (false? (sbs/binary-search test-list 3)))
      (is (true? (sbs/binary-search test-list 13))))
    (is (true? (sbs/binary-search [4 4 5 6 7] 4)))
    (is (false? (sbs/binary-search [4 4 5 6 7] -10)))
    (is (true? (sbs/binary-search [-18 2] -18)))
    (is (true? (sbs/binary-search [5] 5)))
    (is (true? (sbs/binary-search ["a" "c" "d"] "c")))
    (is (false? (sbs/binary-search ["a" "c" "d"] "f")))
    (is (false? (sbs/binary-search [] 1)))
    (is (true? (sbs/binary-search [-0.1 0.1 0.8] 0.1)))
    (is (true? (sbs/binary-search (vec (range -5000 5000 10)) 80)))
    (is (false? (sbs/binary-search (vec (range -5000 5000 10)) 1255)))
    (is (false? (sbs/binary-search (vec (range 0 10000 5)) 2)))))
