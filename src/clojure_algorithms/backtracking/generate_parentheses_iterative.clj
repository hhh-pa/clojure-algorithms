(ns clojure-algorithms.backtracking.generate-parentheses-iterative
  "All valid combinations of `length` pairs of parentheses, built with an
  explicit stack instead of recursion. Time/space: O(2^(2*length)).")

(defn generate-parentheses-iterative
  "Returns all well-formed parenthesis strings of `length` pairs."
  [length]
  (if (zero? length)
    [""]
    (loop [stack [["" 0 0]]
           result []]
      (if (empty? stack)
        result
        (let [[current open-count close-count] (peek stack)
              rest-stack (pop stack)]
          (if (= (count current) (* 2 length))
            (recur rest-stack (conj result current))
            (recur (cond-> rest-stack
                     (< open-count length)
                     (conj [(str current "(") (inc open-count) close-count])
                     (< close-count open-count)
                     (conj [(str current ")") open-count (inc close-count)]))
                   result)))))))
