(ns clojure-algorithms.strings.split
  "Split a string on a separator (manual implementation).
  Ported from TheAlgorithms/Python.")

(defn split
  "Splits `s` on every occurrence of `separator` (default \" \")."
  ([s] (split s " "))
  ([s separator]
   (let [m (count separator)]
     (if (zero? m)
       [s]
       (loop [i 0 last 0 acc []]
         (if (>= i (count s))
           (conj acc (subs s last))
           (if (= (subs s i (min (+ i m) (count s))) separator)
             (recur (+ i m) (+ i m) (conj acc (subs s last i)))
             (recur (inc i) last acc))))))))
