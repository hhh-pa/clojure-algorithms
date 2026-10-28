(ns clojure-algorithms.strings.levenshtein-distance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.levenshtein-distance
             :refer [levenshtein-distance levenshtein-distance-optimized]]))

(def ^:private cases
  [["planet" "planetary" 3] ["" "test" 4] ["book" "back" 2]
   ["book" "book" 0] ["test" "" 4] ["" "" 0]
   ["orchestration" "container" 10]])

(deftest test-levenshtein-variants
  (doseq [f [levenshtein-distance levenshtein-distance-optimized]]
    (testing (str f)
      (doseq [[a b expected] cases]
        (is (= expected (f a b)) (str a " / " b))))))
