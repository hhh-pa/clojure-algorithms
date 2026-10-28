(ns clojure-algorithms.data-structures.arrays.rotate-array
  "Port of data_structures/arrays/rotate_array.py.
  Rotates an array right by STEPS (negative steps rotate left).")

(defn rotate-array
  "Vector of ARR rotated right by STEPS positions."
  [arr steps]
  (let [v (vec arr) n (count v)]
    (if (zero? n)
      []
      (let [k (mod steps n)
            split-at (- n k)]
        (vec (concat (subvec v split-at) (subvec v 0 split-at)))))))
