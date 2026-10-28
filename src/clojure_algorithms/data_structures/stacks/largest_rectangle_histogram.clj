(ns clojure-algorithms.data-structures.stacks.largest-rectangle-histogram
  "Port of data_structures/stacks/largest_rectangle_histogram.py.")

(defn largest-rectangle-area
  "Largest rectangle area in the histogram HEIGHS."
  [highs]
  (let [heights (conj (vec highs) 0)
        n (count heights)]
    (loop [i 0 stack [] max-area 0]
      (if (>= i n)
        max-area
        (let [[stack max-area]
              (loop [stack stack max-area max-area]
                (if (and (seq stack) (< (nth heights i)
                                        (nth heights (peek stack))))
                  (let [h (nth heights (peek stack))
                        stack (pop stack)
                        w (if (empty? stack) i (- i (peek stack) 1))]
                    (recur stack (max max-area (* h w))))
                  [stack max-area]))]
          (recur (inc i) (conj stack i) max-area))))))
