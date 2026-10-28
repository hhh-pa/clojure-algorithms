(ns clojure-algorithms.sorts.shell-sort
  "Shell sort with Marcin Ciura's gap sequence.
  Ported from TheAlgorithms/Python.")

(def gaps
  "Ciura's gap sequence."
  [701 301 132 57 23 10 4 1])

(defn shell-sort
  "Sorts `coll` with shell sort. Returns a sorted vector."
  [coll]
  (reduce (fn [a gap]
            (loop [b a i gap]
              (if (>= i (count b))
                b
                (let [x (nth b i)
                      [b2 j] (loop [c b j i]
                               (if (and (>= j gap)
                                        (neg? (compare x (nth c (- j gap)))))
                                 (recur (assoc c j (nth c (- j gap)))
                                        (- j gap))
                                 [c j]))]
                  (recur (if (= j i) b2 (assoc b2 j x)) (inc i))))))
          (vec coll) gaps))
