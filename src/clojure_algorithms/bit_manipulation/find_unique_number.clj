(ns clojure-algorithms.bit-manipulation.find-unique-number
  "Find the element appearing once in a list where every other element
  appears twice, using bitwise XOR.")

(defn find-unique-number
  "Returns the single non-duplicated integer in `arr`.
  The list must be non-empty and contain only integers."
  [arr]
  (when (empty? arr)
    (throw (ex-info "input list must not be empty" {})))
  (when (not-every? integer? arr)
    (throw (ex-info "all elements must be integers" {})))
  (reduce bit-xor 0 arr))
