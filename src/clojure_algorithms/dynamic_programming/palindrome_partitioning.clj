(ns clojure-algorithms.dynamic-programming.palindrome-partitioning
  "Minimum cuts to partition a string into palindromes.
  Ported from TheAlgorithms/Python
  dynamic_programming/palindrome_partitioning.py.")

(defn find-minimum-partitions
  "Minimum cuts for a palindrome partitioning of `s`."
  [s]
  (let [n (count s)
        cut (atom (vec (repeat n 0)))
        pal? (atom (vec (repeat n (vec (repeat n false)))))]
    (doseq [[i c] (map-indexed vector s)]
      (let [mincut (atom i)]
        (doseq [j (range (inc i))]
          (when (and (= c (nth s j))
                     (or (< (- i j) 2) (get-in @pal? [(inc j) (dec i)])))
            (swap! pal? assoc-in [j i] true)
            (reset! mincut (min @mincut (if (zero? j) 0 (inc (@cut (dec j))))))))
        (swap! cut assoc i @mincut)))
    (@cut (dec n))))
