(ns clojure-algorithms.other.cheap-progress-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.cheap-progress :as c]))

(deftest progress-test
  (testing "yields all items unchanged"
    (is (= [0 1 2 3 4] (vec (c/progress (range 5))))))
  (testing "with desc and total"
    (is (= [0 1 2 3 4] (vec (c/progress (range 5) "Processing" 3)))))
  (testing "negative total treated as unknown"
    (is (= [0 1 2 3 4] (vec (c/progress (range 5) "Processing" -5)))))
  (testing "works on strings"
    (is (= [\a \b \c] (vec (c/progress "abc" "Letters"))))))
