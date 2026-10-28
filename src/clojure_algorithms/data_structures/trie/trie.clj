(ns clojure-algorithms.data-structures.trie.trie
  "Port of data_structures/trie/trie.py.
  Nodes are {:nodes {char node} :is-leaf} maps; a trie is its root.")

(defn make-trie-node
  "Empty trie node."
  []
  {:nodes {} :is-leaf false})

(defn trie-insert
  "Insert WORD; returns the new root."
  [root word]
  (let [root
        (loop [node root chars (seq word) path [:nodes]]
          (if (empty? chars)
            node
            (let [ch (first chars)
                  node (if (get-in node (conj path ch))
                         node
                         (assoc-in node (conj path ch) (make-trie-node)))]
              (recur node (rest chars) (conj path ch :nodes)))))]
    (if (empty? word)
      (assoc root :is-leaf true)
      (let [path (vec (interleave (repeat :nodes) word))]
        (assoc-in root (conj (vec path) :is-leaf) true)))))

(defn trie-insert-many
  "Insert WORDS; returns the new root."
  [root words]
  (reduce trie-insert root words))

(defn trie-find
  "True when WORD is in the trie."
  [root word]
  (loop [node root chars (seq word)]
    (if (empty? chars)
      (boolean (:is-leaf node))
      (let [child (get (:nodes node) (first chars))]
        (if (nil? child)
          false
          (recur child (rest chars)))))))

(defn trie-delete
  "Delete WORD (pruning orphan nodes); returns the new root."
  [root word]
  (letfn [(del [node index]
            (if (= index (count word))
              (if-not (:is-leaf node)
                [node false]
                [(assoc node :is-leaf false)
                 (empty? (:nodes node))])
              (let [ch (nth word index)
                    child (get (:nodes node) ch)]
                (if (nil? child)
                  [node false]
                  (let [[child delete?] (del child (inc index))]
                    (if delete?
                      (let [node (update node :nodes dissoc ch)]
                        [node (and (empty? (:nodes node))
                                   (not (:is-leaf node)))])
                      [(assoc-in node [:nodes ch] child) false]))))))]
    (first (del root 0))))

(defn trie-words
  "All words in the trie (insertion order)."
  ([root] (trie-words root ""))
  ([root prefix]
   (concat (when (:is-leaf root) [prefix])
           (mapcat (fn [[ch child]] (trie-words child (str prefix ch)))
                   (:nodes root)))))
