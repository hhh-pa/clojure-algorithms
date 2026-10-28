(ns clojure-algorithms.data-structures.arrays.reverse-array
  "Port of data_structures/arrays/reverse_array.py.
  Returns a new reversed vector (the Python version reverses in place).")

(defn reverse-array
  "Vector with the items of sequential ARR in reverse order."
  [arr]
  (if (sequential? arr)
    (vec (reverse arr))
    (throw (ex-info (str "'" (type arr) "' object is not reversible here")
                    {:type :type-error}))))
