(ns clojure-algorithms.cellular-automata.elementary-cellular-automaton
  "Wolfram's Rule 30 elementary cellular automaton.")

(defn rule-30-step
  "Computes the next Rule 30 generation of a 0/1 vector."
  [current]
  (let [n (count current)]
    (mapv (fn [i]
            (let [l (if (pos? i) (nth current (dec i)) 0)
                  c (nth current i)
                  r (if (< i (dec n)) (nth current (inc i)) 0)
                  pattern (bit-or (bit-shift-left l 2)
                                  (bit-shift-left c 1) r)]
              (bit-and (bit-shift-right 30 pattern) 1)))
          (range n))))

(defn generate-rule-30
  "Evolves generations of Rule 30 from a single central cell."
  ([] (generate-rule-30 31 15))
  ([size generations]
   (let [first-row (assoc (vec (repeat size 0)) (quot size 2) 1)]
     (vec (take generations (iterate rule-30-step first-row))))))
