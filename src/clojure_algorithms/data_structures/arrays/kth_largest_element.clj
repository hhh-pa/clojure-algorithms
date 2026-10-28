(ns clojure-algorithms.data-structures.arrays.kth-largest-element
  "Port of data_structures/arrays/kth_largest_element.py.
  Quickselect for the kth largest element (descending partition).
  partition-desc returns [pivot-index new-vector].")

(defn partition-desc
  "Partition VEC between LOW and HIGH around the last element (descending).
  Returns [pivot-index new-vector]."
  [v low high]
  (let [pivot (nth v high)
        [i v2] (loop [j low i (dec low) v v]
                 (if (>= j high)
                   [i v]
                   (if (>= (compare (nth v j) pivot) 0)
                     (let [i2 (inc i)]
                       (recur (inc j) i2
                              (assoc v i2 (nth v j) j (nth v i2))))
                     (recur (inc j) i v))))]
    [(inc i) (assoc v2 (inc i) (nth v2 high) high (nth v2 (inc i)))]))

(defn kth-largest-element
  "The POSITION-th largest element of ARR (1-based); -1 when empty."
  [arr position]
  (if (empty? arr)
    -1
    (do
      (when-not (integer? position)
        (throw (ex-info "The position should be an integer"
                        {:type :value-error})))
      (when-not (<= 1 position (count arr))
        (throw (ex-info "Invalid value of 'position'" {:type :value-error})))
      (loop [v (vec arr) low 0 high (dec (count arr))]
        (if (> low high)
          -1
          (if (or (> low (dec (count v))) (neg? high))
            -1
            (let [[pivot-index v2] (partition-desc v low high)]
              (cond
                (= pivot-index (dec position)) (nth v2 pivot-index)
                (> pivot-index (dec position)) (recur v2 low (dec pivot-index))
                :else (recur v2 (inc pivot-index) high)))))))))
