(ns clojure-algorithms.divide-and-conquer.max-subarray
  "Maximum subarray (contiguous, maximum sum) in O(n log n).

  `max-subarray` returns [low high sum]. The matplotlib timing/plotting
  helpers from the Python original are intentionally not ported.
  Ported from TheAlgorithms/Python divide_and_conquer/max_subarray.py.")

(defn max-cross-sum
  "Best subarray crossing `mid`: returns [left right sum]."
  [arr low mid high]
  (let [a (vec arr)
        [left-sum max-left] (loop [i mid best Double/NEGATIVE_INFINITY idx -1 s 0]
                              (if (< i low)
                                [best idx]
                                (let [s (+ s (a i))]
                                  (if (> s best)
                                    (recur (dec i) s i s)
                                    (recur (dec i) best idx s)))))
        [right-sum max-right] (loop [i (inc mid) best Double/NEGATIVE_INFINITY
                                     idx -1 s 0]
                                (if (> i high)
                                  [best idx]
                                  (let [s (+ s (a i))]
                                    (if (> s best)
                                      (recur (inc i) s i s)
                                      (recur (inc i) best idx s)))))]
    [max-left max-right (+ left-sum right-sum)]))

(defn max-subarray
  "Maximum subarray of `arr` between `low` and `high` (inclusive).
  Returns [low high sum]; [nil nil 0] for an empty array."
  [arr low high]
  (let [a (vec arr)]
    (cond
      (empty? a) [nil nil 0]
      (= low high) [low high (a low)]
      :else (let [mid (quot (+ low high) 2)
                  [ll lh ls] (max-subarray a low mid)
                  [rl rh rs] (max-subarray a (inc mid) high)
                  [cl ch cs] (max-cross-sum a low mid high)]
              (cond
                (and (>= ls rs) (>= ls cs)) [ll lh ls]
                (and (>= rs ls) (>= rs cs)) [rl rh rs]
                :else [cl ch cs])))))
