(ns clojure-algorithms.dynamic-programming.longest-common-substring
  "Longest common (contiguous) substring of two strings.
  Ported from TheAlgorithms/Python
  dynamic_programming/longest_common_substring.py.")

(defn longest-common-substring
  "Longest substring present in both `text1` and `text2`."
  [text1 text2]
  (when-not (and (string? text1) (string? text2))
    (throw (IllegalArgumentException.
            "longest_common_substring() takes two strings for inputs")))
  (if (or (empty? text1) (empty? text2))
    ""
    (let [n1 (count text1)
          n2 (count text2)
          dp (atom (vec (repeat (inc n1) (vec (repeat (inc n2) 0)))))
          best (atom [0 0])]
      (doseq [i (range 1 (inc n1))
              j (range 1 (inc n2))]
        (when (= (nth text1 (dec i)) (nth text2 (dec j)))
          (let [v (inc (get-in @dp [(dec i) (dec j)]))]
            (swap! dp assoc-in [i j] v)
            (when (> v (second @best))
              (reset! best [i v])))))
      (let [[end-pos max-len] @best]
        (subs text1 (- end-pos max-len) end-pos)))))
