(ns clojure-algorithms.sorts.strand-sort
  "Strand sort: repeatedly pulls an ordered sublist out of the input and
  merges it into the result. Ported from TheAlgorithms/Python.")

(defn- pull-strand
  "Splits `v` into [strand rest], where the strand is the subsequence of
  elements in (reverse-aware) order starting with the first element."
  [v desc?]
  (let [in-order? (if desc?
                    (fn [item last] (not (pos? (compare item last))))
                    (fn [item last] (not (neg? (compare item last)))))]
    (reduce (fn [[strand rst last] x]
              (if (in-order? x last)
                [(conj strand x) rst x]
                [strand (conj rst x) last]))
            [[(first v)] [] (first v)] (rest v))))

(defn- merge-into
  "Merges sorted `strand` into sorted `solution` (both vectors)."
  [solution strand desc?]
  (loop [s (seq solution) t (seq strand) acc (transient [])]
    (cond
      (nil? s) (persistent! (reduce conj! acc t))
      (nil? t) (persistent! (reduce conj! acc s))
      :else
      (let [c (compare (first t) (first s))
            take-t? (if desc? (pos? c) (neg? c))]
        (if take-t?
          (recur s (next t) (conj! acc (first t)))
          (recur (next s) t (conj! acc (first s))))))))

(defn strand-sort
  "Sorts `coll` with strand sort. Pass :reverse true for descending.
  Returns a sorted vector."
  [coll & {:keys [reverse] :or {reverse false}}]
  (loop [rest-v (vec coll) solution []]
    (if (empty? rest-v)
      solution
      (let [[strand rst] (pull-strand rest-v reverse)]
        (recur rst (if (empty? solution)
                     strand
                     (merge-into solution strand reverse)))))))
