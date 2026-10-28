(ns clojure-algorithms.dynamic-programming.optimal-binary-search-tree
  "Optimal binary search tree (CLRS, with Knuth's optimization).

  Nodes are {:key k :freq f} maps. `find-optimal-bst` returns the
  printed report as a string (the Python original prints it).
  Ported from TheAlgorithms/Python
  dynamic_programming/optimal_binary_search_tree.py.")

(defn- node-str
  "Printable form of a BST node."
  [{:keys [key freq]}]
  (str "Node(key=" key ", freq=" freq ")"))

(defn describe-bst
  "Description lines of the tree encoded by `root-table` over `keys`."
  ([root-table keys i j]
   (describe-bst root-table keys i j ::root false))
  ([root-table keys i j parent-key left?]
   (if (or (> i j) (neg? i) (>= j (count root-table)))
     []
     (let [node (get-in root-table [i j])
           line (cond
                  (= parent-key ::root)
                  (str (nth keys node) " is the root of the binary search tree.")
                  left?
                  (str (nth keys node) " is the left child of key " parent-key ".")
                  :else
                  (str (nth keys node) " is the right child of key " parent-key "."))]
       (into [line]
             (concat (describe-bst root-table keys i (dec node)
                                   (nth keys node) true)
                     (describe-bst root-table keys (inc node) j
                                   (nth keys node) false)))))))

(defn find-optimal-bst
  "Report string for the optimal BST over `nodes`."
  [nodes]
  (let [sorted-nodes (sort-by :key nodes)
        n (count sorted-nodes)
        keys (mapv :key sorted-nodes)
        freqs (mapv :freq sorted-nodes)
        dp (atom (mapv (fn [i] (mapv (fn [j] (if (= i j) (freqs i) 0))
                                     (range n)))
                       (range n)))
        total (atom @dp)
        root (atom (mapv (fn [i] (mapv (fn [j] (if (= i j) i 0)) (range n)))
                         (range n)))]
    (doseq [interval (range 2 (inc n))
            i (range (inc (- n interval)))]
      (let [j (+ i interval -1)]
        (swap! dp assoc-in [i j] Long/MAX_VALUE)
        (swap! total assoc-in [i j] (+ (get-in @total [i (dec j)]) (freqs j)))
        (let [r-start (max i (min (get-in @root [i (dec j)]) j))
              r-end (min j (max (get-in @root [(inc i) j]) i))]
          (doseq [r (range r-start (inc r-end))]
            (let [left (if (> r i) (get-in @dp [i (dec r)]) 0)
                  right (if (< r j) (get-in @dp [(inc r) j]) 0)
                  cost (+ left (get-in @total [i j]) right)]
              (when (> (get-in @dp [i j]) cost)
                (swap! dp assoc-in [i j] cost)
                (swap! root assoc-in [i j] r)))))))
    (apply str
           (concat (interpose "\n" (cons "Binary search tree nodes:"
                                         (map node-str sorted-nodes)))
                   ["\n\nThe cost of optimal BST for given tree nodes is "
                    (str (get-in @dp [0 (dec n)])) ".\n"]
                   (interpose "\n" (describe-bst @root keys 0 (dec n)
                                                 ::root false))
                   ["\n"]))))
