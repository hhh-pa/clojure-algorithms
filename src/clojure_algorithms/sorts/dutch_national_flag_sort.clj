(ns clojure-algorithms.sorts.dutch-national-flag-sort
  "Dutch National Flag sort: single-pass O(n) sort for sequences holding
  only the values 0, 1 and 2. Ported from TheAlgorithms/Python.")

(def colors
  "The three admissible values."
  [0 1 2])

(defn dutch-national-flag-sort
  "Sorts `coll` of 0s, 1s and 2s in one pass. Returns a sorted vector.
  Throws IllegalArgumentException when any element is not 0, 1 or 2."
  [coll]
  (let [v (vec coll)]
    (cond
      (empty? v) []
      (= 1 (count v))
      (do (when-not (contains? #{0 1 2} (first v))
            (throw (IllegalArgumentException.
                    (str "The elements inside the sequence must contains only "
                         colors " values"))))
          v)
      :else
      (loop [a v low 0 mid 0 high (dec (count v))]
        (if (> mid high)
          a
          (let [x (nth a mid)]
            (cond
              (= x 0) (recur (assoc a low (nth a mid) mid (nth a low))
                             (inc low) (inc mid) high)
              (= x 1) (recur a low (inc mid) high)
              (= x 2) (recur (assoc a mid (nth a high) high (nth a mid))
                             low mid (dec high))
              :else (throw (IllegalArgumentException.
                            (str "The elements inside the sequence must contains only "
                                 colors " values"))))))))))
