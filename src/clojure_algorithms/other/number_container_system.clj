(ns clojure-algorithms.other.number-container-system
  "Number container: index <-> number mapping with sorted index lists.

  Port of TheAlgorithms/Python `other/number_container_system.py`.
  Functional: a container is `{:numbermap {num [sorted idxs]}
  :indexmap {idx num}}`; `change` returns the updated container.")

(defn make-container
  "Create an empty number container."
  []
  {:numbermap {} :indexmap {}})

(defn binary-search-delete
  "Remove one occurrence of `item` from sorted vector `coll`.
  Throws when the item is absent."
  [coll item]
  (let [v (vec coll)]
    (loop [low 0 high (dec (count v))]
      (if (> low high)
        (throw (IllegalArgumentException.
                "Either the item is not in the array or the array was unsorted"))
        (let [mid (quot (+ low high) 2)
              m (v mid)]
          (cond
            (= m item) (vec (concat (subvec v 0 mid) (subvec v (inc mid))))
            (< (compare m item) 0) (recur (inc mid) high)
            :else (recur low (dec mid))))))))

(defn binary-search-insert
  "Insert `item` into sorted vector `coll` at its sorted position."
  [coll item]
  (let [v (vec coll)]
    (loop [low 0 high (dec (count v))]
      (if (> low high)
        (vec (concat (subvec v 0 low) [item] (subvec v low)))
        (let [mid (quot (+ low high) 2)
              m (v mid)]
          (cond
            (= m item)
            (vec (concat (subvec v 0 (inc mid)) [item] (subvec v (inc mid))))
            (< (compare m item) 0) (recur (inc mid) high)
            :else (recur low (dec mid))))))))

(defn change
  "Return the container with `index` set to `number`."
  [container index number]
  (let [{:keys [numbermap indexmap]} container
        numbermap (if (contains? indexmap index)
                    (let [old (indexmap index)
                          idxs (numbermap old)]
                      (if (= 1 (count idxs))
                        (dissoc numbermap old)
                        (assoc numbermap old
                               (binary-search-delete idxs index))))
                    numbermap)
        numbermap (if (contains? numbermap number)
                    (assoc numbermap number
                           (binary-search-insert (numbermap number) index))
                    (assoc numbermap number [index]))]
    {:numbermap numbermap
     :indexmap (assoc indexmap index number)}))

(defn find-smallest
  "Smallest index holding `number`, or -1 when absent."
  [container number]
  (get-in container [:numbermap number 0] -1))
