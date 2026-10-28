(ns clojure-algorithms.data-structures.stacks.postfix-evaluation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.postfix-evaluation
             :refer [parse-token evaluate]]))

(deftest test-postfix-eval
  (is (= 0.0 (evaluate ["0"])))
  (is (= -0.0 (evaluate ["-0"])))
  (is (= 1.0 (evaluate ["1"])))
  (is (= -1.0 (evaluate ["-1"])))
  (is (= -1.1 (evaluate ["-1.1"])))
  (is (= 9.0 (evaluate ["2" "1" "+" "3" "*"])))
  (is (= 11.7 (evaluate ["2" "1.9" "+" "3" "*"])))
  (is (= 0.30000000000000027 (evaluate ["2" "-1.9" "+" "3" "*"])))
  (is (= 6.6 (evaluate ["4" "13" "5" "/" "+"])))
  (is (= 1.0 (evaluate ["2" "-" "3" "+"])))
  (is (= -26.0 (evaluate ["-4" "5" "*" "6" "-"])))
  (is (= 0 (evaluate [])))
  (is (thrown? clojure.lang.ExceptionInfo
               (evaluate ["4" "-" "6" "7" "/" "9" "8"])))
  (is (thrown? clojure.lang.ExceptionInfo (parse-token "abc")))
  (is (= "+" (parse-token "+")))
  (is (= 9.0 (evaluate ["2" "1" "+" "3" "*"] true))))
