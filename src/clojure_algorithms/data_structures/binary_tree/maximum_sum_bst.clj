(ns clojure-algorithms.data-structures.binary-tree.maximum-sum-bst
  "Port of data_structures/binary_tree/maximum_sum_bst.py.
  Maximum sum over all BST subtrees.")

(def int-min (+ Long/MIN_VALUE 2))
(def int-max (dec Long/MAX_VALUE))

(defn make-node
  "Create a node with VAL and optional LEFT/RIGHT children."
  ([val] (make-node val nil nil))
  ([val left right] {:val val :left left :right right}))

(defn- solver [node]
  (if (nil? node)
    [true int-max int-min 0 0]
    (let [[lv? lmin lmax lsum lbest] (solver (:left node))
          [rv? rmin rmax rsum rbest] (solver (:right node))]
      (if (and lv? rv? (< lmax (:val node) rmin))
        (let [total (+ lsum rsum (:val node))]
          [true (min lmin (:val node)) (max rmax (:val node)) total
           (max lbest rbest total)])
        [false -1 -1 -1 (max lbest rbest)]))))

(defn max-sum-bst
  "Maximum sum of any BST subtree (0 when none is positive)."
  [root]
  (nth (solver root) 4))
