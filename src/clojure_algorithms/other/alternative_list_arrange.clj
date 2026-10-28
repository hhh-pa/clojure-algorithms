(ns clojure-algorithms.other.alternative-list-arrange
  "Interleave two collections element-by-element.

  Port of TheAlgorithms/Python `other/alternative_list_arrange.py`.")

(defn alternative-list-arrange
  "Arrange two collections as one list in alternating order.
  Extra elements of the longer collection are appended at the end."
  [first-coll second-coll]
  (let [a (vec first-coll)
        b (vec second-coll)
        n (max (count a) (count b))]
    (vec (mapcat (fn [i]
                   (cond-> []
                     (< i (count a)) (conj (a i))
                     (< i (count b)) (conj (b i))))
                 (range n)))))
