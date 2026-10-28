(ns clojure-algorithms.greedy-methods.optimal-merge-pattern
  "Optimal merge pattern: repeatedly merge the two smallest files so the
  total merge cost is minimal.")

(defn optimal-merge-pattern
  "Returns the optimal (minimum) cost of merging all files."
  [files]
  (loop [files (vec files)
         cost 0]
    (if (<= (count files) 1)
      cost
      (let [sorted-files (sort files)
            merged (+ (first sorted-files) (second sorted-files))]
        (recur (conj (vec (drop 2 sorted-files)) merged)
               (+ cost merged))))))
