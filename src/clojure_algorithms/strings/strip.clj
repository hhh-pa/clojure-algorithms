(ns clojure-algorithms.strings.strip
  "Strip leading/trailing characters (manual implementation).
  Ported from TheAlgorithms/Python.")

(defn strip
  "Removes leading/trailing chars in `characters` (default whitespace)."
  ([s] (strip s " \t\n\r"))
  ([s characters]
   (let [cs (set characters)
         n (count s)
         start (loop [i 0]
                 (if (and (< i n) (contains? cs (nth s i)))
                   (recur (inc i)) i))
         end (loop [i n]
               (if (and (> i start) (contains? cs (nth s (dec i))))
                 (recur (dec i)) i))]
     (subs s start end))))
