(ns clojure-algorithms.bit-manipulation.gray-code-sequence
  "n-bit Gray code sequences: 2^n values where consecutive entries
  (including last-to-first) differ by exactly one bit.")

(defn gray-code-sequence-string
  "Returns the `bit-count`-bit Gray code sequence as bit strings."
  [bit-count]
  (cond
    (zero? bit-count) ["0"]
    (= 1 bit-count) ["0" "1"]
    :else (let [smaller (gray-code-sequence-string (dec bit-count))
                half (count smaller)]
            (vec (concat (map #(str "0" %) (take half smaller))
                         (map #(str "1" %) (reverse (take half smaller))))))))

(defn gray-code
  "Returns the `bit-count`-bit Gray code sequence as integers.
  `bit-count` must be non-negative."
  [bit-count]
  (when (neg? bit-count)
    (throw (ex-info "The given input must be positive" {})))
  (mapv #(Integer/parseInt % 2) (gray-code-sequence-string bit-count)))
