(ns clojure-algorithms.data-structures.binary-tree.wavelet-tree
  "Port of data_structures/binary_tree/wavelet_tree.py.
  Static wavelet tree over small integer arrays.
  Nodes are {:minn :maxx :map-left :left :right} maps.")

(def test-array
  [2 1 4 5 6 0 8 9 1 2 0 6 4 2 0 6 5 3 2 7])

(defn make-node
  "Create an empty wavelet node for LENGTH elements."
  [length]
  {:minn -1 :maxx -1 :map-left (vec (repeat length -1))
   :left nil :right nil})

(defn node->str
  "Render NODE like the Python repr."
  [node]
  (str "Node(min_value=" (:minn node) " max_value=" (:maxx node) ")"))

(defn build-tree
  "Build the wavelet tree over ARR."
  [arr]
  (let [v (vec arr)]
    (when (seq v)
      (let [mn (apply min v) mx (apply max v)]
        (if (= mn mx)
          (assoc (make-node (count v)) :minn mn :maxx mx)
          (let [pivot (quot (+ mn mx) 2)
                [left-arr right-arr map-left]
                (loop [i 0 l [] r [] ml []]
                  (if (>= i (count v))
                    [l r ml]
                    (let [num (nth v i)
                          l (if (<= num pivot) (conj l num) l)
                          r (if (<= num pivot) r (conj r num))]
                      (recur (inc i) l r (conj ml (count l))))))]
            (assoc (make-node (count v))
                   :minn mn :maxx mx :map-left (vec map-left)
                   :left (build-tree left-arr)
                   :right (build-tree right-arr))))))))

(defn rank-till-index
  "Occurrences of NUM in arr[0..INDEX]."
  [node num index]
  (cond
    (or (neg? index) (nil? node)) 0
    (= (:minn node) (:maxx node)) (if (= (:minn node) num) (inc index) 0)
    :else (let [pivot (quot (+ (:minn node) (:maxx node)) 2)]
            (if (<= num pivot)
              (rank-till-index (:left node) num
                               (dec (nth (:map-left node) index)))
              (rank-till-index (:right node) num
                               (- index (nth (:map-left node) index)))))))

(defn wavelet-rank
  "Occurrences of NUM in arr[START..END]."
  [node num start end]
  (if (> start end)
    0
    (- (rank-till-index node num end)
       (rank-till-index node num (dec start)))))

(defn quantile
  "INDEX-th smallest (0-based) in arr[START..END]; -1 when invalid."
  [node index start end]
  (cond
    (or (> index (- end start)) (> start end) (nil? node)) -1
    (= (:minn node) (:maxx node)) (:minn node)
    :else
    (let [ml (:map-left node)
          left-count (- (nth ml end) (if (pos? start) (nth ml (dec start)) 0))]
      (if (> left-count index)
        (quantile (:left node) index
                  (if (pos? start) (nth ml (dec start)) 0)
                  (dec (nth ml end)))
        (quantile (:right node) (- index left-count)
                  (- start (if (pos? start) (nth ml (dec start)) 0))
                  (- end (nth ml end)))))))

(defn range-counting
  "Count of values in [START-NUM, END-NUM] within arr[START..END]."
  [node start end start-num end-num]
  (cond
    (or (> start end) (nil? node) (> start-num end-num)
        (> (:minn node) end-num) (< (:maxx node) start-num)) 0
    (and (<= start-num (:minn node)) (<= (:maxx node) end-num))
    (inc (- end start))
    :else
    (let [ml (:map-left node)
          before (if (pos? start) (nth ml (dec start)) 0)]
      (+ (range-counting (:left node) before (dec (nth ml end))
                         start-num end-num)
         (range-counting (:right node) (- start before)
                         (- end (nth ml end)) start-num end-num)))))
