(ns clojure-algorithms.dynamic-programming.word-break
  "Whether a string segments into words from a list (trie + memoized DP).
  Ported from TheAlgorithms/Python dynamic_programming/word_break.py.")

(defn- insert-word
  "Trie with `word` inserted; terminal nodes carry :word?."
  [trie word]
  (update-in trie (vec (seq word)) #(assoc (or % {}) :word? true)))

(defn word-break?
  "True when `s` segments into one or more words from `words`."
  [s words]
  (when-not (and (string? s) (seq s))
    (throw (IllegalArgumentException. "the string should be not empty string")))
  (when-not (and (sequential? words)
                 (every? #(and (string? %) (seq %)) words))
    (throw (IllegalArgumentException.
            "the words should be a list of non-empty strings")))
  (let [trie (reduce insert-word {} words)
        n (count s)
        memo (atom {})]
    (letfn [(breakable? [index]
              (cond
                (= index n) true
                (contains? @memo index) (@memo index)
                :else (let [result (loop [i index node trie]
                                     (let [child (and (< i n)
                                                      (get node (nth s i)))]
                                       (cond
                                         (nil? child) false
                                         (and (:word? child)
                                              (breakable? (inc i))) true
                                         :else (recur (inc i) child))))]
                        (swap! memo assoc index result)
                        result)))]
      (boolean (breakable? 0)))))
