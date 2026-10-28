(ns clojure-algorithms.data-structures.suffix-tree.suffix-tree-node
  "Port of data_structures/suffix_tree/suffix_tree_node.py.
  Nodes are {:children :is-end-of-string :start :end :suffix-link} maps.")

(defn make-suffix-tree-node
  "Create a suffix tree node."
  ([] (make-suffix-tree-node {} false nil nil nil))
  ([children is-end-of-string start end suffix-link]
   {:children (or children {})
    :is-end-of-string is-end-of-string
    :start start :end end
    :suffix-link suffix-link}))
