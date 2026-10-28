(ns clojure-algorithms.strings.top-k-frequent-words
  "Top-K most frequent words.

  The Python original uses the repo's Heap class; this port orders by
  descending frequency, which is the observable behaviour.
  Ported from TheAlgorithms/Python.")

(defn top-k-frequent-words
  "The `k` most frequent words in `words`, most frequent first."
  [words k]
  (->> (frequencies words)
       (sort-by val >)
       (take (min k (count (set words))))
       (mapv key)))
