(ns clojure-algorithms.graphics.butterfly-pattern-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphics.butterfly-pattern
             :refer [butterfly-pattern]]))

(deftest test-butterfly-pattern
  (is (= "*   *\n** **\n*****\n** **\n*   *" (butterfly-pattern 3)))
  (is (= 9 (count (clojure.string/split-lines (butterfly-pattern 5)))))
  (is (= "*********" (nth (clojure.string/split-lines (butterfly-pattern 5)) 4))))
