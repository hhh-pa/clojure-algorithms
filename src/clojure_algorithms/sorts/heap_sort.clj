(ns clojure-algorithms.sorts.heap-sort
  "Heap sort. Ported from TheAlgorithms/Python.")

(defn heapify
  "Restores the max-heap property at `index` for the heap of `heap-size`
  elements in vector `v`. Returns the updated vector."
  [v index heap-size]
  (let [largest (let [l (inc (* 2 index))
                      r (+ 2 (* 2 index))
                      m (if (and (< l heap-size)
                                 (neg? (compare (nth v index) (nth v l))))
                          l index)]
                  (if (and (< r heap-size)
                           (neg? (compare (nth v m) (nth v r))))
                    r m))]
    (if (= largest index)
      v
      (recur (assoc v largest (nth v index) index (nth v largest))
             largest heap-size))))

(defn heap-sort
  "Sorts `coll` with heap sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)
        heap (reduce (fn [a i] (heapify a i n))
                     v (range (dec (quot n 2)) -1 -1))]
    (loop [a heap i (dec n)]
      (if (<= i 0)
        a
        (recur (heapify (assoc a 0 (nth a i) i (nth a 0)) 0 i)
               (dec i))))))
