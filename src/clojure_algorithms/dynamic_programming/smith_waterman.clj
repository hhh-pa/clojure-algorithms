(ns clojure-algorithms.dynamic-programming.smith-waterman
  "Local sequence alignment with Smith-Waterman (linear gap penalty).
  Ported from TheAlgorithms/Python dynamic_programming/smith_waterman.py.")

(defn score-function
  "Pair score: `match` on equality, `mismatch` otherwise, `gap` for gaps."
  ([source-char target-char]
   (score-function source-char target-char 1 -1 -2))
  ([source-char target-char match mismatch gap]
   (if (or (= \- source-char) (= \- target-char))
     gap
     (if (= source-char target-char) match mismatch))))

(defn smith-waterman
  "Score matrix (vector of vectors) aligning `query` to `subject`."
  ([query subject]
   (smith-waterman query subject 1 -1 -2))
  ([query subject match mismatch gap]
   (let [q (.toUpperCase ^String query)
         s (.toUpperCase ^String subject)
         m (count q)
         n (count s)
         score (atom (vec (repeat (inc m) (vec (repeat (inc n) 0)))))]
     (doseq [i (range 1 (inc m))
             j (range 1 (inc n))]
       (swap! score assoc-in [i j]
              (max 0
                   (+ (get-in @score [(dec i) (dec j)])
                      (score-function (nth q (dec i)) (nth s (dec j))
                                      match mismatch gap))
                   (+ (get-in @score [(dec i) j]) gap)
                   (+ (get-in @score [i (dec j)]) gap))))
     @score)))

(defn traceback
  "Optimal local alignment lines \"top\\nbottom\" from a score matrix."
  [score query subject]
  (let [q (.toUpperCase ^String query)
        s (.toUpperCase ^String subject)
        gap (score-function \- \-)
        [[max-i max-j]] (reduce (fn [[[bi bj] bv :as best] [i j v]]
                                  (if (> v bv) [[i j] v] best))
                                [[0 0] Double/NEGATIVE_INFINITY]
                                (for [i (range (count score))
                                      j (range (count (first score)))]
                                  [i j (get-in score [i j])]))]
    (if (or (zero? max-i) (zero? max-j))
      ""
      (loop [i max-i j max-j top "" bottom ""]
        (if (or (zero? i) (zero? j))
          (str top "\n" bottom)
          (cond
            (= (get-in score [i j])
               (+ (get-in score [(dec i) (dec j)])
                  (score-function (nth q (dec i)) (nth s (dec j)))))
            (recur (dec i) (dec j)
                   (str (nth q (dec i)) top) (str (nth s (dec j)) bottom))

            (= (get-in score [i j])
               (+ (get-in score [(dec i) j]) gap))
            (recur (dec i) j (str (nth q (dec i)) top) (str "-" bottom))

            :else
            (recur i (dec j) (str "-" top) (str (nth s (dec j)) bottom))))))))
