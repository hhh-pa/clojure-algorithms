(ns clojure-algorithms.dynamic-programming.needleman-wunsch
  "Global sequence alignment with the Needleman-Wunsch algorithm.
  Returns [aligned-seq1 aligned-seq2 score].
  Ported from TheAlgorithms/Python dynamic_programming/needleman_wunsch.py.")

(defn needleman-wunsch
  "Optimal global alignment of `sequence1` and `sequence2`."
  ([sequence1 sequence2]
   (needleman-wunsch sequence1 sequence2 1 -1 -1))
  ([sequence1 sequence2 match-score mismatch-score gap-score]
   (when (pos? gap-score)
     (throw (IllegalArgumentException.
             "gap_score must be non-positive (<= 0)")))
   (let [m (count sequence1)
         n (count sequence2)
         score (atom (vec (repeat (inc m) (vec (repeat (inc n) 0)))))]
     (doseq [i (range (inc m))]
       (swap! score assoc-in [i 0] (* i gap-score)))
     (doseq [j (range (inc n))]
       (swap! score assoc-in [0 j] (* j gap-score)))
     (doseq [i (range 1 (inc m))
             j (range 1 (inc n))]
       (let [sub (if (= (nth sequence1 (dec i)) (nth sequence2 (dec j)))
                   match-score mismatch-score)]
         (swap! score assoc-in [i j]
                (max (+ (get-in @score [(dec i) (dec j)]) sub)
                     (+ (get-in @score [(dec i) j]) gap-score)
                     (+ (get-in @score [i (dec j)]) gap-score)))))
     (loop [row m col n acc1 [] acc2 []]
       (if (and (zero? row) (zero? col))
         [(apply str (reverse acc1)) (apply str (reverse acc2))
          (get-in @score [m n])]
         (let [sub (when (and (pos? row) (pos? col))
                     (if (= (nth sequence1 (dec row)) (nth sequence2 (dec col)))
                       match-score mismatch-score))]
           (cond
             (and (pos? row) (pos? col)
                  (= (get-in @score [row col])
                     (+ (get-in @score [(dec row) (dec col)]) sub)))
             (recur (dec row) (dec col)
                    (conj acc1 (nth sequence1 (dec row)))
                    (conj acc2 (nth sequence2 (dec col))))

             (and (pos? row)
                  (= (get-in @score [row col])
                     (+ (get-in @score [(dec row) col]) gap-score)))
             (recur (dec row) col
                    (conj acc1 (nth sequence1 (dec row))) (conj acc2 \-))

             :else
             (recur row (dec col) (conj acc1 \-)
                    (conj acc2 (nth sequence2 (dec col)))))))))))
