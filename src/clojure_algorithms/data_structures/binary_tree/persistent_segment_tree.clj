(ns clojure-algorithms.data-structures.binary-tree.persistent-segment-tree
  "Port of data_structures/binary_tree/persistent_segment_tree.py.
  Versioned segment tree for range sums. State is {:n :roots}.")

(defn- build-nodes [arr start end]
  (if (= start end)
    {:value (nth arr start) :left nil :right nil}
    (let [mid (quot (+ start end) 2)
          left (build-nodes arr start mid)
          right (build-nodes arr (inc mid) end)]
      {:value (+ (:value left) (:value right)) :left left :right right})))

(defn make-pst
  "Persistent segment tree over ARR (version 0)."
  [arr]
  {:n (count arr) :roots [(build-nodes (vec arr) 0 (dec (count arr)))]})

(defn- update-node [node start end index value]
  (if (= start end)
    {:value value :left nil :right nil}
    (let [mid (quot (+ start end) 2)
          [left right] (if (<= index mid)
                         [(update-node (:left node) start mid index value)
                          (:right node)]
                         [(:left node)
                          (update-node (:right node) (inc mid) end index value)])]
      {:value (+ (:value left) (if right (:value right) 0))
       :left left :right right})))

(defn pst-update
  "Set INDEX to VALUE in VERSION; returns [new-state new-version]."
  [{:keys [n roots] :as pst} version index value]
  (let [new-root (update-node (nth roots version) 0 (dec n) index value)]
    [(assoc pst :roots (conj roots new-root)) (count roots)]))

(defn- query-node [node start end left right]
  (cond
    (or (nil? node) (> left end) (< right start)) 0
    (and (<= left start) (>= right end)) (:value node)
    :else (let [mid (quot (+ start end) 2)]
            (+ (query-node (:left node) start mid left right)
               (query-node (:right node) (inc mid) end left right)))))

(defn pst-query
  "Range sum over [LEFT, RIGHT] in VERSION."
  [{:keys [n roots]} version left right]
  (query-node (nth roots version) 0 (dec n) left right))
