(ns clojure-algorithms.backtracking.n-queens-math-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.n-queens-math
             :refer [depth-first-search n-queens-solutions]]))

(deftest depth-first-search-test
  (is (= [[". Q . . " ". . . Q " "Q . . . " ". . Q . "]
          [". . Q . " "Q . . . " ". . . Q " ". Q . . "]]
         (depth-first-search [] #{} #{} 4))))

(deftest n-queens-solutions-test
  (testing "known solution counts"
    (is (= 2 (count (n-queens-solutions 4))))
    (is (= 1 (count (n-queens-solutions 1))))
    (is (= 0 (count (n-queens-solutions 2))))
    (is (= 0 (count (n-queens-solutions 3)))))
  (testing "every solution holds n non-attacking queens"
    (doseq [board (n-queens-solutions 5)]
      (is (= 5 (count board)))
      (is (= 5 (count (filter #(.contains ^String % "Q") board)))))))
