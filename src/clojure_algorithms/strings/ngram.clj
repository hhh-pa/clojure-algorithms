(ns clojure-algorithms.strings.ngram
  "Character n-grams. Ported from TheAlgorithms/Python.")

(defn create-ngram
  "All character n-grams of size `n` in `sentence`."
  [sentence n]
  (if (> n (count sentence))
    []
    (mapv #(subs sentence % (+ % n))
          (range (inc (- (count sentence) n))))))
