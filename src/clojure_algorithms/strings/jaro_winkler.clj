(ns clojure-algorithms.strings.jaro-winkler
  "Jaro-Winkler string similarity.
  See https://en.wikipedia.org/wiki/Jaro-Winkler_distance
  Ported from TheAlgorithms/Python.")

(defn- matched-characters
  "Characters of `s1` that match characters of `s2` within the Jaro window."
  [s1 s2]
  (let [positions (reduce (fn [m [i ch]]
                            (update m ch (fnil conj []) i))
                          {} (map-indexed vector s2))
        limit (quot (min (count s1) (count s2)) 2)]
    (loop [i 0 pos positions acc []]
      (if (>= i (count s1))
        (apply str acc)
        (let [ch (nth s1 i)
              ps (get pos ch)]
          (if (empty? ps)
            (recur (inc i) pos acc)
            (let [left (max 0 (- i limit))
                  right (min (+ i limit 1) (count s2))
                  ps2 (drop-while (fn [x] (< x left)) ps)]
              (if (and (seq ps2) (< (first ps2) right))
                (recur (inc i) (assoc pos ch (vec (rest ps2)))
                       (conj acc ch))
                (recur (inc i) (assoc pos ch (vec ps2)) acc)))))))))

(defn jaro-winkler
  "Jaro-Winkler similarity of `s1` and `s2`, between 0.0 and 1.0."
  [s1 s2]
  (let [m1 (matched-characters s1 s2)
        m2 (matched-characters s2 s1)
        mc (count m1)
        transpositions (quot (count (filter false? (map = m1 m2))) 2)
        jaro (if (zero? mc)
               0.0
               (* (/ 1 3.0)
                  (+ (/ mc (count s1)) (/ mc (count s2))
                     (/ (- mc transpositions) mc))))
        prefix (count (take-while true?
                                  (map = (take 4 s1) (take 4 s2))))]
    (+ jaro (* 0.1 prefix (- 1 jaro)))))
