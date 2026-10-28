(ns clojure-algorithms.data-structures.disjoint-set.alternate-disjoint-set
  "Port of data_structures/disjoint_set/alternate_disjoint_set.py.
  Array-based union-find tracking set sizes.
  State is {:set-counts [...] :max-set n :ranks [...] :parents [...] }.")

(defn make-disjoint-set
  "Disjoint-set state over SET-COUNTS."
  [set-counts]
  {:set-counts (vec set-counts)
   :max-set (apply max set-counts)
   :ranks (vec (repeat (count set-counts) 1))
   :parents (vec (range (count set-counts)))})

(defn get-parent
  "Vector [new-state root] for DISJ-SET with path compression."
  [{:keys [parents] :as ds} disj-set]
  (if (= (nth parents disj-set) disj-set)
    [ds disj-set]
    (let [[ds root] (get-parent ds (nth parents disj-set))]
      [(assoc-in ds [:parents disj-set] root) root])))

(defn ds-merge
  "Union SRC and DST; returns [new-state merged?]."
  [ds src dst]
  (let [[ds src-parent] (get-parent ds src)
        [ds dst-parent] (get-parent ds dst)]
    (if (= src-parent dst-parent)
      [ds false]
      (let [{:keys [ranks set-counts]} ds]
        (if (>= (nth ranks dst-parent) (nth ranks src-parent))
          (let [joined (+ (nth set-counts dst-parent)
                          (nth set-counts src-parent))
                ds (-> ds
                       (assoc-in [:set-counts dst-parent] joined)
                       (assoc-in [:set-counts src-parent] 0)
                       (assoc-in [:parents src-parent] dst-parent))
                ds (if (= (nth ranks dst-parent) (nth ranks src-parent))
                     (update-in ds [:ranks dst-parent] inc)
                     ds)]
            [(assoc ds :max-set (max (:max-set ds) joined)) true])
          (let [joined (+ (nth set-counts src-parent)
                          (nth set-counts dst-parent))
                ds (-> ds
                       (assoc-in [:set-counts src-parent] joined)
                       (assoc-in [:set-counts dst-parent] 0)
                       (assoc-in [:parents dst-parent] src-parent))]
            [(assoc ds :max-set (max (:max-set ds) joined)) true]))))))
