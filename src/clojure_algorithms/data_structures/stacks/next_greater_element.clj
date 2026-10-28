(ns clojure-algorithms.data-structures.stacks.next-greater-element
  "Port of data_structures/stacks/next_greater_element.py.
  Next greater element, three ways.")

(def arr [-10 -5 0 5 5.1 11 13 21 3 4 -21 -10 -5 -1 0])
(def expect [-5 0 5 5.1 11 13 21 -1 4 -1 -10 -5 -1 0 -1])

(defn next-greatest-element-slow
  "Brute-force NGE (-1 when none)."
  [a]
  (mapv (fn [i]
          (or (first (filter #(< (nth a i) %) (subvec (vec a) (inc i))))
              -1))
        (range (count a))))

(defn next-greatest-element-fast
  "Readable brute-force NGE (-1 when none)."
  [a]
  (mapv (fn [i outer]
          (or (first (filter #(< outer %) (subvec (vec a) (inc i)))) -1))
        (range (count a)) a))

(defn next-greatest-element
  "Stack-based O(n) NGE (-1 when none)."
  [a]
  (let [n (count a)]
    (loop [index (dec n) stack [] result (vec (repeat n -1))]
      (if (neg? index)
        result
        (let [stack (loop [stack stack]
                      (if (and (seq stack)
                               (<= (peek stack) (nth a index)))
                        (recur (pop stack))
                        stack))
              result (if (seq stack)
                       (assoc result index (peek stack))
                       result)]
          (recur (dec index) (conj stack (nth a index)) result))))))
