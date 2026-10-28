(ns clojure-algorithms.backtracking.word-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.word-search
             :refer [get-point-key exits-word validate-board-and-word
                     get-word-path word-exists]]))

(def ^:private board
  [["A" "B" "C" "E"] ["S" "F" "C" "S"] ["A" "D" "E" "E"]])

(deftest get-point-key-test
  (is (= 200 (get-point-key 10 20 1 0))))

(deftest exits-word-test
  (is (false? (exits-word [["A"]] "B" 0 0 0 #{}))))

(deftest validate-board-and-word-test
  (is (nil? (validate-board-and-word board "ABCCED")))
  (is (nil? (validate-board-and-word board "SEE")))
  (is (nil? (validate-board-and-word board "ABCB")))
  (is (nil? (validate-board-and-word [["A"]] "A")))
  (is (nil? (validate-board-and-word [["B" "A" "A"] ["A" "A" "A"] ["A" "B" "A"]] "ABB")))
  (is (thrown? clojure.lang.ExceptionInfo (validate-board-and-word [["A"]] 123)))
  (is (thrown? clojure.lang.ExceptionInfo (validate-board-and-word [["A"]] "")))
  (is (thrown? clojure.lang.ExceptionInfo (validate-board-and-word [[]] "AB")))
  (is (thrown? clojure.lang.ExceptionInfo (validate-board-and-word [] "AB")))
  (is (thrown? clojure.lang.ExceptionInfo (validate-board-and-word [["A"] [21]] "AB"))))

(deftest get-word-path-test
  (is (= [[0 0] [0 1] [0 2] [1 2] [2 2] [2 1]]
         (get-word-path board "ABCCED")))
  (is (= [[1 3] [2 3] [2 2]] (get-word-path board "SEE")))
  (is (nil? (get-word-path board "ABCB")))
  (is (thrown? clojure.lang.ExceptionInfo (get-word-path [["A"]] 123))))

(deftest word-exists-test
  (is (true? (word-exists board "ABCCED")))
  (is (true? (word-exists board "SEE")))
  (is (false? (word-exists board "ABCB")))
  (is (true? (word-exists [["A"]] "A")))
  (is (false? (word-exists [["B" "A" "A"] ["A" "A" "A"] ["A" "B" "A"]] "ABB")))
  (is (thrown? clojure.lang.ExceptionInfo (word-exists [["A"]] 123)))
  (is (thrown? clojure.lang.ExceptionInfo (word-exists [["A"]] "")))
  (is (thrown? clojure.lang.ExceptionInfo (word-exists [[]] "AB")))
  (is (thrown? clojure.lang.ExceptionInfo (word-exists [] "AB")))
  (is (thrown? clojure.lang.ExceptionInfo (word-exists [["A"] [21]] "AB"))))
