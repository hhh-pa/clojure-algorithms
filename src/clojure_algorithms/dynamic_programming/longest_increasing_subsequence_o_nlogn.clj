(ns clojure-algorithms.dynamic-programming.longest-increasing-subsequence-o-nlogn
  "Length of the longest strictly increasing subsequence in O(n log n).
  Ported from TheAlgorithms/Python
  dynamic_programming/longest_increasing_subsequence_o_nlogn.py.")

(defn ceil-index
  "Insertion point for `key` in `v` between `left` and `right`."
  [v left right key]
  (loop [l left r right]
    (if (> (- r l) 1)
      (let [middle (quot (+ l r) 2)]
        (if (>= (nth v middle) key)
          (recur l middle)
          (recur middle r)))
      r)))

(defn longest-increasing-subsequence-length
  "Length of the longest strictly increasing subsequence of `v`."
  [v]
  (let [arr (vec v)]
    (if (empty? arr)
      0
      (loop [i 1 tail (assoc (vec (repeat (count arr) 0)) 0 (arr 0)) length 1]
        (if (>= i (count arr))
          length
          (cond
            (< (arr i) (tail 0))
            (recur (inc i) (assoc tail 0 (arr i)) length)

            (> (arr i) (tail (dec length)))
            (recur (inc i) (assoc tail length (arr i)) (inc length))

            :else
            (recur (inc i)
                   (assoc tail (ceil-index tail -1 (dec length) (arr i)) (arr i))
                   length)))))))
