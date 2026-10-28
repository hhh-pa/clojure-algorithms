(ns clojure-algorithms.dynamic-programming.regex-match
  "Regex matching with `.` (any char) and `*` (zero or more of
  the preceding element): recursive and DP variants.
  Ported from TheAlgorithms/Python dynamic_programming/regex_match.py.")

(defn- drop-last-2
  "String without its last two characters (or \"\" when shorter)."
  [s]
  (subs s 0 (max 0 (- (count s) 2))))

(defn recursive-match
  "True when `text` fully matches `pattern` (recursive)."
  [text pattern]
  (cond
    (empty? pattern) (empty? text)

    (empty? text)
    (and (= \* (last pattern))
         (recursive-match text (drop-last-2 pattern)))

    (or (= (last text) (last pattern)) (= \. (last pattern)))
    (recursive-match (subs text 0 (dec (count text)))
                     (subs pattern 0 (dec (count pattern))))

    (= \* (last pattern))
    (or (recursive-match (subs text 0 (dec (count text))) pattern)
        (recursive-match text (drop-last-2 pattern)))

    :else false))

(defn dp-match
  "True when `text` fully matches `pattern` (dynamic programming)."
  [text pattern]
  (let [m (count text)
        n (count pattern)
        dp (atom (vec (repeat (inc m) (vec (repeat (inc n) false)))))]
    (swap! dp assoc-in [0 0] true)
    (doseq [j (range 1 (inc n))]
      (swap! dp assoc-in [0 j]
             (and (= \* (nth pattern (dec j)))
                  (get-in @dp [0 (- j 2)]))))
    (doseq [i (range 1 (inc m))
            j (range 1 (inc n))]
      (swap! dp assoc-in [i j]
             (cond
               (contains? #{\. (nth text (dec i))} (nth pattern (dec j)))
               (get-in @dp [(dec i) (dec j)])

               (= \* (nth pattern (dec j)))
               (let [star (get-in @dp [i (- j 2)])]
                 (if (contains? #{\. (nth text (dec i))}
                                (nth pattern (- j 2)))
                   (or star (get-in @dp [(dec i) j]))
                   star))

               :else false)))
    (get-in @dp [m n])))
