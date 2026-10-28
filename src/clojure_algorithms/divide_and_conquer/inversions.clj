(ns clojure-algorithms.divide-and-conquer.inversions
  "Count inversions (i < j with A[i] > A[j]) in a collection.

  Provides the naive O(n^2) algorithm and the divide-and-conquer
  O(n log n) algorithm.
  Ported from TheAlgorithms/Python divide_and_conquer/inversions.py.")

(defn count-inversions-bf
  "Count inversions with the naive brute-force algorithm."
  [coll]
  (let [arr (vec coll)
        n (count arr)]
    (reduce (fn [total i]
              (+ total (count (filter #(> (arr i) %) (subvec arr (inc i))))))
            0
            (range (max 0 (dec n))))))

(defn count-cross-inversions
  "Merge sorted `p` and `q`, returning [merged cross-inversion-count]."
  [p q]
  (let [p (vec p)
        q (vec q)]
    (loop [i 0 j 0 merged [] n 0]
      (cond
        (>= i (count p)) [(into merged (subvec q j)) n]
        (>= j (count q)) [(into merged (subvec p i)) n]
        (> (p i) (q j)) (recur i (inc j) (conj merged (q j))
                               (+ n (- (count p) i)))
        :else (recur (inc i) j (conj merged (p i)) n)))))

(defn count-inversions-recursive
  "Count inversions with divide and conquer.
  Returns [sorted-coll inversion-count]."
  [coll]
  (let [arr (vec coll)]
    (if (<= (count arr) 1)
      [arr 0]
      (let [mid (quot (count arr) 2)
            [a inv-p] (count-inversions-recursive (subvec arr 0 mid))
            [b inv-q] (count-inversions-recursive (subvec arr mid))
            [merged cross] (count-cross-inversions a b)]
        [merged (+ inv-p inv-q cross)]))))
