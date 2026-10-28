(ns clojure-algorithms.data-structures.arrays.permutations
  "Port of data_structures/arrays/permutations.py.
  All permutations via rotation-recursion or backtracking.")

(defn permute-recursive
  "All permutations of NUMS (rotation order)."
  [nums]
  (if (empty? nums)
    [[]]
    (loop [n (count nums) v (vec nums) result []]
      (if (zero? n)
        result
        (let [head (first v)
              rest-v (vec (rest v))
              perms (mapv #(conj % head) (permute-recursive rest-v))]
          (recur (dec n) (conj rest-v head) (into result perms)))))))

(defn permute-backtrack
  "All permutations of NUMS (lexicographic backtracking order)."
  [nums]
  (letfn [(backtrack [start v]
            (if (= start (dec (count v)))
              [v]
              (mapcat (fn [i]
                        (let [swapped (assoc v start (nth v i)
                                             i (nth v start))]
                          (backtrack (inc start) swapped)))
                      (range start (count v)))))]
    (if (empty? nums) [[]] (vec (backtrack 0 (vec nums))))))
