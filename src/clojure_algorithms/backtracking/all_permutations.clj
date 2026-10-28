(ns clojure-algorithms.backtracking.all-permutations
  "All permutations of a sequence via a backtracking state-space tree (DFS).
  Time complexity: O(n! * n).
  Unlike the Python reference, which prints each permutation, the functions
  here return them as a vector.")

(defn create-state-space-tree
  "DFS over the permutation state space. Returns a vector of all completions
  of `current-sequence` using unused elements of `sequence`."
  [sequence current-sequence index index-used]
  (if (= index (count sequence))
    [current-sequence]
    (vec (mapcat (fn [i]
                   (if (nth index-used i)
                     []
                     (create-state-space-tree sequence
                                              (conj current-sequence (nth sequence i))
                                              (inc index)
                                              (assoc index-used i true))))
                 (range (count sequence))))))

(defn generate-all-permutations
  "Returns all permutations of `sequence` as a vector of vectors."
  [sequence]
  (create-state-space-tree (vec sequence) [] 0
                           (vec (repeat (count sequence) false))))
