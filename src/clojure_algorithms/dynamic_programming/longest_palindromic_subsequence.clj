(ns clojure-algorithms.dynamic-programming.longest-palindromic-subsequence
  "Length of the longest palindromic subsequence (LCS against the
  reversed string).
  Ported from TheAlgorithms/Python
  dynamic_programming/longest_palindromic_subsequence.py.")

(defn longest-palindromic-subsequence
  "Length of the longest palindromic subsequence of `s`."
  [s]
  (let [n (count s)
        rev (apply str (reverse s))
        dp (atom (vec (repeat (inc n) (vec (repeat (inc n) 0)))))]
    (doseq [i (range 1 (inc n))
            j (range 1 (inc n))]
      (swap! dp assoc-in [i j]
             (if (= (nth s (dec i)) (nth rev (dec j)))
               (inc (get-in @dp [(dec i) (dec j)]))
               (max (get-in @dp [(dec i) j]) (get-in @dp [i (dec j)])))))
    (get-in @dp [n n])))
