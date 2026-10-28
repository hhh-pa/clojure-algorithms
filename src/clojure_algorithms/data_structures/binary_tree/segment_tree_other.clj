(ns clojure-algorithms.data-structures.binary-tree.segment-tree-other
  "Port of data_structures/binary_tree/segment_tree_other.py.
  Generic segment tree over node maps {:start :end :mid :val :left :right}
  with a caller-supplied combine function F.")

(defn- build-tree [coll f start end]
  (if (= start end)
    {:start start :end end :mid start :val (nth coll start)
     :left nil :right nil}
    (let [mid (quot (+ start end) 2)
          left (build-tree coll f start mid)
          right (build-tree coll f (inc mid) end)]
      {:start start :end end :mid mid :val (f (:val left) (:val right))
       :left left :right right})))

(defn make-seg-tree
  "Segment tree over COLL with combine function F."
  [coll f]
  {:coll (vec coll) :f f
   :root (when (seq coll) (build-tree (vec coll) f 0 (dec (count coll))))})

(defn- update-tree [node f i val]
  (if (and (= (:start node) i) (= (:end node) i))
    (assoc node :val val)
    (let [node (if (<= i (:mid node))
                 (assoc node :left (update-tree (:left node) f i val))
                 (assoc node :right (update-tree (:right node) f i val)))]
      (assoc node :val (f (:val (:left node)) (:val (:right node)))))))

(defn seg-update
  "Set index I to VAL; returns the new tree."
  [{:keys [f root] :as st} i val]
  (assoc st :root (update-tree root f i val)
         :coll (assoc (:coll st) i val)))

(defn- query-range-at [node f i j]
  (cond
    (and (= (:start node) i) (= (:end node) j)) (:val node)
    (<= i (:mid node))
    (if (<= j (:mid node))
      (query-range-at (:left node) f i j)
      (f (query-range-at (:left node) f i (:mid node))
         (query-range-at (:right node) f (inc (:mid node)) j)))
    :else (query-range-at (:right node) f i j)))

(defn query-range
  "Combine over inclusive [I, J]."
  [{:keys [f root]} i j]
  (query-range-at root f i j))

(defn traverse
  "Breadth-first node sequence."
  [{:keys [root]}]
  (when root
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY root) out []]
      (if (empty? queue)
        out
        (let [node (peek queue)]
          (recur (cond-> (pop queue)
                   (:left node) (conj (:left node))
                   (:right node) (conj (:right node)))
                 (conj out node)))))))

(defn node->str
  "Render NODE like the Python repr."
  [node]
  (str "SegmentTreeNode(start=" (:start node) ", end=" (:end node)
       ", val=" (:val node) ")"))
