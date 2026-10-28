(ns clojure-algorithms.maths.kth-lexicographic-permutation
  "Port of maths/kth_lexicographic_permutation.py")

(defn kth-permutation
  "Returns the k'th lexicographic permutation (in increasing order, 0-based)
  of 0,1,2,...n-1 in O(n^2) time."
  [k n]
  (let [factorials (vec (reductions *' 1 (range 2 n)))]
    (assert (and (<= 0 k) (< k (* (peek factorials) n))) "k out of bounds")
    (loop [k k, facts (rseq factorials), elems (vec (range n)), perm []]
      (if (empty? facts)
        (conj perm (first elems))
        (let [f (first facts)
              q (quot k f)
              r (rem k f)]
          (recur r
                 (next facts)
                 (vec (concat (subvec elems 0 q) (subvec elems (inc q))))
                 (conj perm (nth elems q))))))))
