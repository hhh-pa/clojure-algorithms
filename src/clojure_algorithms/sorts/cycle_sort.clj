(ns clojure-algorithms.sorts.cycle-sort
  "Cycle sort: minimizes writes by rotating disjoint cycles into place.
  Ported from TheAlgorithms/Python.")

(defn- cycle-pos
  "Position where `item` (from `start`) belongs: start plus the count of
  smaller elements after it, skipping over equals (only when the rank
  position differs from `start`, exactly like the Python original)."
  [v start item n]
  (let [rank (count (filter #(neg? (compare % item))
                            (subvec v (inc start) n)))
        p (+ start rank)]
    (if (= p start)
      p
      (loop [q p]
        (if (and (< q n) (= item (nth v q)))
          (recur (inc q))
          q)))))

(defn cycle-sort
  "Sorts `coll` with cycle sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v cycle-start 0]
      (if (>= cycle-start (dec n))
        a
        (let [item (nth a cycle-start)
              pos (cycle-pos a cycle-start item n)]
          (if (= pos cycle-start)
            (recur a (inc cycle-start))
            (let [displaced (nth a pos)
                  a2 (assoc a pos item)]
              (recur (loop [b a2 it displaced]
                       (let [p2 (cycle-pos b cycle-start it n)
                             b2 (assoc b p2 it)]
                         (if (= p2 cycle-start)
                           b2
                           (recur b2 (nth b p2)))))
                     (inc cycle-start)))))))))
