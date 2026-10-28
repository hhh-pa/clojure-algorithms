(ns clojure-algorithms.dynamic-programming.edit-distance
  "Edit (Levenshtein) distance: top-down and bottom-up dynamic programming.
  Ported from TheAlgorithms/Python dynamic_programming/edit_distance.py.")

(defn min-dist-top-down
  "Edit distance between `word1` and `word2` via memoized recursion."
  [word1 word2]
  (let [memo (atom {})]
    (letfn [(dist [m n]
              (cond
                (= m -1) (inc n)
                (= n -1) (inc m)
                (contains? @memo [m n]) (@memo [m n])
                :else (let [d (if (= (nth word1 m) (nth word2 n))
                                (dist (dec m) (dec n))
                                (inc (min (dist m (dec n))
                                          (dist (dec m) n)
                                          (dist (dec m) (dec n)))))]
                        (swap! memo assoc [m n] d)
                        d)))]
      (dist (dec (count word1)) (dec (count word2))))))

(defn min-dist-bottom-up
  "Edit distance between `word1` and `word2` via tabulation."
  [word1 word2]
  (let [m (count word1)
        n (count word2)]
    (loop [i 0 dp (mapv (fn [r] (mapv (fn [c]
                                        (cond (zero? r) c
                                              (zero? c) r
                                              :else 0))
                                      (range (inc n))))
                        (range (inc m)))]
      (if (> i m)
        (get-in dp [m n])
        (recur (inc i)
               (reduce (fn [d j]
                         (if (or (zero? i) (zero? j))
                           d
                           (assoc-in d [i j]
                                     (if (= (nth word1 (dec i))
                                            (nth word2 (dec j)))
                                       (get-in d [(dec i) (dec j)])
                                       (inc (min (get-in d [i (dec j)])
                                                 (get-in d [(dec i) j])
                                                 (get-in d [(dec i) (dec j)])))))))
                       dp
                       (range (inc n))))))))
