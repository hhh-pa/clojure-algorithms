(ns clojure-algorithms.data-structures.binary-tree.lowest-common-ancestor
  "Port of data_structures/binary_tree/lowest_common_ancestor.py.
  Binary-lifting LCA over an adjacency map.")

(defn swap-pair
  "Swap A and B (the Python version uses xor swaps)."
  [a b]
  [b a])

(defn create-sparse
  "Fill the binary-lifting PARENT table up to MAX-NODE."
  [max-node parent]
  (loop [p (vec (map vec parent)) j 1]
    (if (>= (bit-shift-left 1 j) max-node)
      p
      (recur (reduce (fn [p i]
                       (assoc-in p [j i]
                                 (get-in p [(dec j) (get-in p [(dec j) i])])))
                     p (range 1 (inc max-node)))
             (inc j)))))

(defn lowest-common-ancestor
  "Lowest common ancestor of U and V given LEVEL and PARENT table."
  [u v level parent]
  (let [[u v] (if (< (nth level u) (nth level v)) [v u] [u v])
        u (loop [u u i 18]
            (if (< i 0)
              u
              (recur (if (>= (- (nth level u) (bit-shift-left 1 i))
                             (nth level v))
                       (get-in parent [i u]) u)
                     (dec i))))]
    (if (= u v)
      u
      (let [[u v] (loop [u u v v i 18]
                    (if (< i 0)
                      [u v]
                      (let [pu (get-in parent [i u])
                            pv (get-in parent [i v])]
                        (if (and (not= pu 0) (not= pu pv))
                          (recur pu pv (dec i))
                          (recur u v (dec i))))))]
        (get-in parent [0 u])))))

(defn breadth-first-search
  "BFS from ROOT; returns [level parent-table]."
  [level parent max-node graph root]
  (let [level (assoc (vec level) root 0)]
    (loop [level level parent (vec (map vec parent))
           queue (conj clojure.lang.PersistentQueue/EMPTY root)]
      (if (empty? queue)
        [level parent]
        (let [u (peek queue)
              queue (pop queue)
              [level parent queue]
              (reduce (fn [[level parent queue] v]
                        (if (= (nth level v) -1)
                          [(assoc level v (inc (nth level u)))
                           (assoc-in parent [0 v] u)
                           (conj queue v)]
                          [level parent queue]))
                      [level parent queue] (get graph u []))]
          (recur level parent queue))))))

(defn sample-graph-data
  "The [level parent] tables for the doc's sample graph."
  []
  (let [max-node 13
        parent (vec (repeat 20 (vec (repeat (+ max-node 10) 0))))
        level (vec (repeat (+ max-node 10) -1))
        graph {1 [2 3 4] 2 [5] 3 [6 7] 4 [8] 5 [9 10] 6 [11] 7 []
               8 [12 13] 9 [] 10 [] 11 [] 12 [] 13 []}
        [level parent] (breadth-first-search level parent max-node graph 1)]
    [level (create-sparse max-node parent)]))
