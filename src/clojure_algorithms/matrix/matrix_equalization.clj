(ns clojure-algorithms.matrix.matrix-equalization
  "Equalizes all elements of a vector to a common value with the minimal
  number of updates under a step-size constraint.")

(defn array-equalization
  "Returns the minimal number of updates needed to equalize every element
  of vector, where one update fixes a position and skips step-size ahead."
  [vector step-size]
  (when (<= step-size 0)
    (throw (ex-info "Step size must be positive and non-zero."
                    {:type :ValueError})))
  (when-not (integer? step-size)
    (throw (ex-info "Step size must be an integer." {:type :ValueError})))
  (let [v (vec vector)
        n (count v)]
    (reduce min
            Long/MAX_VALUE
            (for [element (distinct v)]
              (loop [idx 0 updates 0]
                (if (>= idx n)
                  updates
                  (if (= (nth v idx) element)
                    (recur (inc idx) updates)
                    (recur (+' idx step-size) (inc updates)))))))))
