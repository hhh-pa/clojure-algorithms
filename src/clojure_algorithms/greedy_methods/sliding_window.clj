(ns clojure-algorithms.greedy-methods.sliding-window
  "Longest substring without repeating characters via the sliding-window
  technique (O(n) time).")

(defn sliding-window
  "Returns the length of the longest substring of input-string with all
  distinct characters. Throws on non-string input."
  [input-string]
  (when-not (string? input-string)
    (throw (ex-info "Input must be a string" {:input input-string})))
  (loop [chars (map-indexed vector input-string)
         last-seen {}
         left 0
         max-len 0]
    (if (empty? chars)
      max-len
      (let [[right ch] (first chars)
            prev (get last-seen ch -1)
            left (if (>= prev left) (inc prev) left)]
        (recur (rest chars)
               (assoc last-seen ch right)
               left
               (max max-len (- right left -1)))))))
