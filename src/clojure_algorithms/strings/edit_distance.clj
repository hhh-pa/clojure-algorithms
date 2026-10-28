(ns clojure-algorithms.strings.edit-distance
  "Classic edit distance (insert/delete/substitute cost 1), memoized.
  The Python original uses naive recursion; this port memoizes the same
  recurrence so longer inputs stay tractable.
  Ported from TheAlgorithms/Python.")

(declare edit-distance)

(defn- ed
  [source target]
  (cond
    (empty? source) (count target)
    (empty? target) (count source)
    :else
    (let [delta (if (= (peek source) (peek target)) 0 1)]
      (min (+ (edit-distance (pop source) (pop target)) delta)
           (inc (edit-distance source (pop target)))
           (inc (edit-distance (pop source) target))))))

(def edit-distance
  "Minimum operations turning `source` into `target`."
  (memoize (fn [source target] (ed (vec source) (vec target)))))
