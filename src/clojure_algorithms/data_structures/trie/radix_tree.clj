(ns clojure-algorithms.data-structures.trie.radix-tree
  "Port of data_structures/trie/radix_tree.py.
  Space-optimized trie with edge compression.
  Nodes are {:nodes {first-char node} :is-leaf :prefix} maps.")

(defn make-radix-node
  "Radix node with PREFIX (default \"\")."
  ([] (make-radix-node "" false))
  ([prefix] (make-radix-node prefix false))
  ([prefix is-leaf] {:nodes {} :is-leaf is-leaf :prefix prefix}))

(defn node-match
  "Vector [common remaining-prefix remaining-word] for PREFIX and WORD."
  [prefix word]
  (let [x (count (take-while true? (map = prefix word)))]
    [(subs prefix 0 x) (subs prefix x) (subs word x)]))

(defn radix-insert
  "Insert WORD into NODE; returns the new node."
  [node word]
  (cond
    (empty? word) (assoc node :is-leaf true)
    (and (= (:prefix node) word) (not (:is-leaf node)))
    (assoc node :is-leaf true)
    (not (contains? (:nodes node) (first word)))
    (assoc-in node [:nodes (first word)]
              (make-radix-node word true))
    :else
    (let [incoming (get-in node [:nodes (first word)])
          [matching remaining-prefix remaining-word]
          (node-match (:prefix incoming) word)]
      (if (empty? remaining-prefix)
        (assoc-in node [:nodes (first matching)]
                  (radix-insert (get-in node [:nodes (first matching)])
                                remaining-word))
        (let [aux (assoc incoming :prefix remaining-prefix)
              mid (assoc (make-radix-node matching false)
                         :nodes {(first remaining-prefix) aux})
              mid (if (empty? remaining-word)
                    (assoc mid :is-leaf true)
                    (radix-insert mid remaining-word))]
          (assoc-in node [:nodes (first matching)] mid))))))

(defn radix-insert-many
  "Insert WORDS; returns the new root."
  [node words]
  (reduce radix-insert node words))

(defn radix-find
  "True when WORD is in the tree."
  [node word]
  (if (empty? word)
    (boolean (:is-leaf node))
    (let [incoming (get (:nodes node) (first word))]
      (if (nil? incoming)
        false
        (let [[_ remaining-prefix remaining-word]
              (node-match (:prefix incoming) word)]
          (cond
            (not (empty? remaining-prefix)) false
            (empty? remaining-word) (boolean (:is-leaf incoming))
            :else (radix-find incoming remaining-word)))))))

(defn radix-delete
  "Delete WORD; returns [deleted? new-node]."
  [node word]
  (if (empty? word)
    [false node]
    (let [incoming (get (:nodes node) (first word))]
      (if (nil? incoming)
        [false node]
        (let [[_ remaining-prefix remaining-word]
              (node-match (:prefix incoming) word)]
          (cond
            (not (empty? remaining-prefix)) [false node]
            (not (empty? remaining-word))
            (let [[deleted? new-incoming] (radix-delete incoming remaining-word)]
              [deleted? (assoc-in node [:nodes (first word)] new-incoming)])
            (not (:is-leaf incoming)) [false node]
            (empty? (:nodes incoming))
            (let [node (update node :nodes dissoc (first word))]
              (if (and (= 1 (count (:nodes node))) (not (:is-leaf node)))
                (let [merging (val (first (:nodes node)))]
                  [true (-> node
                            (assoc :is-leaf (:is-leaf merging))
                            (update :prefix str (:prefix merging))
                            (assoc :nodes (:nodes merging)))])
                [true node]))
            (> (count (:nodes incoming)) 1)
            [true (assoc-in node [:nodes (first word) :is-leaf] false)]
            :else
            (let [merging (val (first (:nodes incoming)))
                  new-incoming (-> incoming
                                   (assoc :is-leaf (:is-leaf merging))
                                   (update :prefix str (:prefix merging))
                                   (assoc :nodes (:nodes merging)))]
              [true (assoc-in node [:nodes (first word)] new-incoming)])))))))

(defn tree-str
  "Render the tree like print_tree."
  ([node] (tree-str node 0))
  ([node height]
   (clojure.string/join
    "\n"
    (concat (when (not (empty? (:prefix node)))
              [(str (apply str (repeat height \-)) " " (:prefix node)
                    (when (:is-leaf node) "   (leaf)"))])
            (mapcat #(clojure.string/split-lines (tree-str % (inc height)))
                    (vals (:nodes node)))))))
