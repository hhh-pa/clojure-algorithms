(ns clojure-algorithms.strings.alternative-string-arrange
  "Interleave two strings character by character.
  Ported from TheAlgorithms/Python.")

(defn alternative-string-arrange
  "Merges `a` and `b` alternating characters; the longer tail is appended."
  [a b]
  (let [n (max (count a) (count b))]
    (apply str (mapcat (fn [i]
                         (concat (when (< i (count a)) [(nth a i)])
                                 (when (< i (count b)) [(nth b i)])))
                       (range n)))))
