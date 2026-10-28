(ns clojure-algorithms.backtracking.generate-parentheses-iterative-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.generate-parentheses-iterative
             :refer [generate-parentheses-iterative]]))

(deftest generate-parentheses-iterative-test
  (testing "doctest examples"
    (is (= ["()()()" "()(())" "(())()" "(()())" "((()))"]
           (generate-parentheses-iterative 3)))
    (is (= ["()()" "(())"] (generate-parentheses-iterative 2)))
    (is (= ["()"] (generate-parentheses-iterative 1)))
    (is (= [""] (generate-parentheses-iterative 0))))
  (testing "Catalan count and balancedness"
    (let [result (generate-parentheses-iterative 4)]
      (is (= 14 (count result)))
      (doseq [s result]
        (is (= 8 (count s)))
        (is (zero? (reduce (fn [bal ch]
                             (+ bal (if (= ch \() 1 -1)))
                           0 s)))))))
