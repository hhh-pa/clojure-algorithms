(ns clojure-algorithms.backtracking.word-ladder-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.word-ladder :refer [backtrack word-ladder]]))

(deftest backtrack-test
  (is (= ["hit" "hot" "dot" "lot" "log" "cog"]
         (backtrack "hit" ["hit"] "cog" #{"hot" "dot" "dog" "lot" "log" "cog"})))
  (is (= [] (backtrack "hit" ["hit"] "cog" #{"hot" "dot" "dog" "lot" "log"})))
  (is (= ["lead" "lead" "load" "goad" "gold"]
         (backtrack "lead" ["lead"] "gold" #{"load" "goad" "gold" "lead" "lord"})))
  (is (= ["game" "came" "cade" "code"]
         (backtrack "game" ["game"] "code" #{"came" "cage" "code" "cade" "gave"}))))

(deftest word-ladder-test
  (is (= ["hit" "hot" "dot" "lot" "log" "cog"]
         (word-ladder "hit" "cog" ["hot" "dot" "dog" "lot" "log" "cog"])))
  (is (= [] (word-ladder "hit" "cog" ["hot" "dot" "dog" "lot" "log"])))
  (is (= ["lead" "lead" "load" "goad" "gold"]
         (word-ladder "lead" "gold" ["load" "goad" "gold" "lead" "lord"])))
  (is (= ["game" "came" "cade" "code"]
         (word-ladder "game" "code" ["came" "cage" "code" "cade" "gave"]))))
