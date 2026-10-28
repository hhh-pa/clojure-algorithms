(ns clojure-algorithms.backtracking.generate-parentheses
  "All well-formed combinations of n pairs of parentheses via recursion.
  See https://leetcode.com/problems/generate-parentheses/description/
  Time: O(2^(2n)); space: O(n).")

(defn backtrack
  "Explores completions of `partial` (`open-count`/`close-count` placed so
  far) and returns the vector of complete combinations."
  [partial open-count close-count n]
  (if (= (count partial) (* 2 n))
    [partial]
    (vec (concat (when (< open-count n)
                   (backtrack (str partial "(") (inc open-count) close-count n))
                 (when (< close-count open-count)
                   (backtrack (str partial ")") open-count (inc close-count) n))))))

(defn generate-parenthesis
  "Returns all well-formed parenthesis strings of `n` pairs."
  [n]
  (backtrack "" 0 0 n))
