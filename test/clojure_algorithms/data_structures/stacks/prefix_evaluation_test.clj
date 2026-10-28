(ns clojure-algorithms.data-structures.stacks.prefix-evaluation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.prefix-evaluation
             :refer [is-operand evaluate evaluate-recursive operators]]))

(deftest test-prefix-eval
  (is (true? (is-operand "1")))
  (is (false? (is-operand "+")))
  (is (= 21 (evaluate "+ 9 * 2 6")))
  (is (= 4.0 (evaluate "/ * 10 2 + 4 1 ")))
  (is (= 2 (evaluate "2")))
  (is (= 8.0 (evaluate "+ * 2 3 / 8 4")))
  (is (= [2 []] [(first (evaluate-recursive ["2"]))
                 (second (evaluate-recursive ["2"]))]))
  (let [[v rest-toks] (evaluate-recursive ["+" "*" "2" "3" "/" "8" "4"])]
    (is (= 8.0 v))
    (is (= [] rest-toks)))
  (is (= 21 (first (evaluate-recursive ["+" "9" "*" "2" "6"]))))
  (is (= 4.0 (first (evaluate-recursive ["/" "*" "10" "2" "+" "4" "1"])))))
