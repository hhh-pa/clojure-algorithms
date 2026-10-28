(ns clojure-algorithms.backtracking.crossword-puzzle-solver-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.crossword-puzzle-solver
             :refer [valid? place-word remove-word solve-crossword]]))

(def ^:private empty-4x4 (vec (repeat 4 (vec (repeat 4 "")))))

(deftest valid-test
  (is (true? (valid? empty-4x4 "word" 0 0 true)))
  (is (true? (valid? empty-4x4 "word" 0 0 false)))
  (let [puzzle2 [["w" "" ""] ["o" "" ""] ["r" "" ""] ["d" "" ""]]]
    (is (true? (valid? puzzle2 "word" 0 0 true)))
    (is (false? (valid? puzzle2 "cat" 0 0 true))))
  (testing "out-of-bounds placement is invalid"
    (is (false? (valid? empty-4x4 "toolong" 0 0 false)))))

(deftest place-word-test
  (is (= [["w" "" "" ""] ["o" "" "" ""] ["r" "" "" ""] ["d" "" "" ""]]
         (place-word empty-4x4 "word" 0 0 true))))

(deftest remove-word-test
  (let [puzzle [["w" "o" "r" "d"] ["" "" "" ""] ["" "" "" ""] ["" "" "" ""]]
        snap [["" "o" "r" "d"] ["" "" "" ""] ["" "" "" ""] ["" "" "" ""]]]
    (is (= snap (remove-word puzzle "word" 0 0 false snap)))))

(deftest solve-crossword-test
  (testing "solvable puzzle"
    (let [solved (solve-crossword empty-4x4 ["word" "four" "more" "last"])]
      (is (some? solved))
      (is (= 4 (count solved)))
      (is (every? #(= 4 (count %)) solved))))
  (testing "unsolvable puzzle returns nil"
    (is (nil? (solve-crossword empty-4x4 ["word" "four" "more" "paragraphs"]))))
  (testing "3x3 doctest-style case"
    (let [puzzle (vec (repeat 3 (vec (repeat 3 ""))))
          solved (solve-crossword puzzle ["cat" "dog" "car"])]
      (is (some? solved)))))
