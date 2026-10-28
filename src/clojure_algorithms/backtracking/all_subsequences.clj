(ns clojure-algorithms.backtracking.all-subsequences
  "All subsequences of a sequence via a backtracking state-space tree (each
  state has two children: skip or take the element).
  Time complexity: O(2^n).
  Unlike the Python reference, which prints each subsequence, the functions
  here return them as a vector.")

(defn create-state-space-tree
  "DFS over the subsequence state space starting at `index`. Returns a vector
  of all subsequences extending `current-subsequence`."
  [sequence current-subsequence index]
  (if (= index (count sequence))
    [current-subsequence]
    (vec (concat (create-state-space-tree sequence current-subsequence (inc index))
                 (create-state-space-tree sequence
                                          (conj current-subsequence (nth sequence index))
                                          (inc index))))))

(defn generate-all-subsequences
  "Returns all 2^n subsequences of `sequence` as a vector of vectors."
  [sequence]
  (create-state-space-tree (vec sequence) [] 0))
