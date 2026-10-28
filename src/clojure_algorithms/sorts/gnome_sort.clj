(ns clojure-algorithms.sorts.gnome-sort
  "Gnome sort (stupid sort). Ported from TheAlgorithms/Python.")

(defn gnome-sort
  "Sorts `coll` with gnome sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      v
      (loop [a v i 1]
        (if (>= i (count a))
          a
          (if (not (neg? (compare (nth a i) (nth a (dec i)))))
            (recur a (inc i))
            (recur (assoc a (dec i) (nth a i) i (nth a (dec i)))
                   (if (zero? (dec i)) 1 (dec i)))))))))
