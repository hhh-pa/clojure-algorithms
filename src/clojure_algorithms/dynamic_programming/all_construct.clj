(ns clojure-algorithms.dynamic-programming.all-construct
  "All ways to build `target` from substrings in `word-bank` (tabulation).
  Ported from TheAlgorithms/Python dynamic_programming/all_construct.py.")

(defn all-construct
  "Vector of all word combinations that concatenate to `target`."
  ([target] (all-construct target []))
  ([target word-bank]
   (let [size (inc (count target))]
     (loop [i 0 table (assoc (vec (repeat size [])) 0 [[]])]
       (if (>= i size)
         (mapv #(vec (reverse %)) (table (count target)))
         (recur (inc i)
                (if (empty? (table i))
                  table
                  (reduce (fn [t word]
                            (if (= (subs target i (min (count target)
                                                       (+ i (count word))))
                                   word)
                              (update t (+ i (count word)) into
                                      (map #(cons word %) (t i)))
                              t))
                          table
                          word-bank))))))))
