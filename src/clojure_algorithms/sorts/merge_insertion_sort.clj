(ns clojure-algorithms.sorts.merge-insertion-sort
  "Merge-insertion (Ford-Johnson) sort.
  Ported from TheAlgorithms/Python.")

(defn binary-search-insertion
  "Inserts `item` into sorted vector `v` at its binary-searched position.
  Returns the updated vector."
  [v item]
  (let [n (count v)]
    (loop [left 0 right (dec n)]
      (if (> left right)
        (into (conj (subvec v 0 left) item) (subvec v left))
        (let [middle (quot (+ left right) 2)]
          (cond
            (and (= left right)
                 (not (neg? (compare (nth v middle) item))))
            (recur left (dec right))

            (neg? (compare (nth v middle) item))
            (recur (inc middle) right)

            :else
            (recur left (dec middle))))))))

(defn merge-pairs
  "Merges two sorted seqs of pairs by each pair's first element."
  [left right]
  (loop [l (seq left) r (seq right) acc []]
    (cond
      (nil? l) (into acc r)
      (nil? r) (into acc l)
      (neg? (compare (ffirst (list l)) (ffirst (list r))))
      (recur (next l) r (conj acc (first l)))
      :else
      (recur l (next r) (conj acc (first r))))))

(defn sortlist-2d
  "Merge-sorts a seq of pairs by first element."
  [pairs]
  (let [v (vec pairs)]
    (if (<= (count v) 1)
      v
      (let [mid (quot (count v) 2)]
        (merge-pairs (sortlist-2d (subvec v 0 mid))
                     (sortlist-2d (subvec v mid)))))))

(defn merge-insertion-sort
  "Sorts `coll` with merge-insertion sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      v
      (let [has-odd? (odd? (count v))
            pairs (vec (for [i (range 0 (- (count v) (if has-odd? 1 0)) 2)]
                         (let [a (nth v i) b (nth v (inc i))]
                           (if (neg? (compare a b)) [a b] [b a]))))
            sorted-pairs (sortlist-2d pairs)
            result (conj (mapv first sorted-pairs) (second (peek sorted-pairs)))
            result2 (if has-odd?
                      (binary-search-insertion result (peek v))
                      result)
            last-val (peek v)]
        (loop [res result2 i 0 inserted-before? false]
          (if (>= i (dec (count sorted-pairs)))
            res
            (let [inserted? (or inserted-before?
                                (and has-odd? (= (nth res i) last-val)))
                  pivot (second (nth sorted-pairs i))
                  cut (if inserted? (+ i 2) (+ i 1))]
              (recur (into (subvec res 0 (min cut (count res)))
                           (binary-search-insertion
                            (subvec res (min cut (count res))) pivot))
                     (inc i) inserted?))))))))
