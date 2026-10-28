(ns clojure-algorithms.dynamic-programming.longest-repeating-subsequence
  "Length of the longest subsequence occurring at least twice (using
  different positions).
  Ported from TheAlgorithms/Python
  dynamic_programming/longest_repeating_subsequence.py.")

(defn longest-repeating-subsequence
  "Length of the longest repeating subsequence of string `s`."
  [s]
  (when-not (string? s)
    (throw (IllegalArgumentException.
            (str "Input must be a string, got "
                 (if (nil? s) "nil" (.getSimpleName (class s)))))))
  (let [n (count s)]
    (if (zero? n)
      0
      (let [dp (atom (vec (repeat (inc n) (vec (repeat (inc n) 0)))))]
        (doseq [i (range 1 (inc n))
                j (range 1 (inc n))]
          (swap! dp assoc-in [i j]
                 (if (and (= (nth s (dec i)) (nth s (dec j))) (not= i j))
                   (inc (get-in @dp [(dec i) (dec j)]))
                   (max (get-in @dp [(dec i) j]) (get-in @dp [i (dec j)])))))
        (get-in @dp [n n])))))
