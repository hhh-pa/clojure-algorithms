(ns clojure-algorithms.other.bankers-algorithm
  "Banker's algorithm: deadlock-avoidance safety check.

  Port of TheAlgorithms/Python `other/bankers_algorithm.py`, stdlib-only
  (no numpy). Pure functions return the safe execution order instead of
  printing it.")

(def test-claim-vector [8 5 9 7])

(def test-allocated-resources-table
  [[2 0 1 1]
   [0 1 2 1]
   [4 0 0 3]
   [0 2 1 0]
   [1 0 3 0]])

(def test-maximum-claim-table
  [[3 2 1 4]
   [0 2 5 2]
   [5 1 0 5]
   [1 5 3 0]
   [3 0 3 3]])

(defn processes-resource-summation
  "Column-wise sums of the allocated-resources table."
  [allocated-table]
  (apply mapv + allocated-table))

(defn available-resources
  "Resources still free: claim vector minus allocated sums."
  [claim-vector allocated-table]
  (mapv - claim-vector (processes-resource-summation allocated-table)))

(defn need-table
  "Remaining need per process: max claim minus allocation."
  [max-claim-table allocated-table]
  (mapv (fn [max-row alloc-row] (mapv - max-row alloc-row))
        max-claim-table allocated-table))

(defn- can-execute?
  "True when every need in `need-row` fits in `available`."
  [need-row available]
  (every? true? (map <= need-row available)))

(defn safe-sequence
  "Return a safe execution order (0-based process indices) or nil when the
  system is in an unsafe state."
  [claim-vector allocated-table max-claim-table]
  (let [needs (need-table max-claim-table allocated-table)
        available (available-resources claim-vector allocated-table)]
    (loop [needs (vec needs)
           idxs (vec (range (count needs)))
           avail (vec available)
           order []]
      (if (empty? needs)
        order
        (let [pos (first (keep-indexed (fn [k row]
                                         (when (can-execute? row avail) k))
                                       needs))]
          (if (nil? pos)
            nil
            (let [p (idxs pos)
                  freed (mapv + avail (allocated-table p))]
              (recur (vec (concat (subvec needs 0 pos) (subvec needs (inc pos))))
                     (vec (concat (subvec idxs 0 pos) (subvec idxs (inc pos))))
                     freed
                     (conj order p)))))))))

(defn safe-state?
  "True when a safe execution order exists."
  [claim-vector allocated-table max-claim-table]
  (some? (safe-sequence claim-vector allocated-table max-claim-table)))
