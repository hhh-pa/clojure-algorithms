(ns clojure-algorithms.strings.autocomplete-using-trie
  "Prefix autocomplete over a trie.
  Ported from TheAlgorithms/Python.")

(def end-marker
  "Trie key marking the end of an inserted word."
  :end)

(defn insert-word
  "Inserts `word` into trie map `trie`. Returns the updated trie."
  [trie word]
  (assoc-in trie (concat (seq word) [end-marker]) true))

(defn- suffixes
  "All suffix completions below trie node `node`."
  [node]
  (mapcat (fn [[ch sub]]
            (if (= ch end-marker)
              [" "]
              (map #(str ch %) (suffixes sub))))
          node))

(defn find-word
  "Suffixes completing `prefix` in `trie`, or [] when absent."
  [trie prefix]
  (let [node (get-in trie (seq prefix) ::missing)]
    (if (= node ::missing)
      []
      (vec (suffixes node)))))

(def default-words
  ["depart" "detergent" "daring" "dog" "deer" "deal"])

(def default-trie
  (reduce insert-word {} default-words))

(defn autocomplete-using-trie
  "All completions of `prefix` from the default word set."
  ([prefix] (autocomplete-using-trie default-trie prefix))
  ([trie prefix]
   (mapv #(str prefix %) (find-word trie prefix))))
