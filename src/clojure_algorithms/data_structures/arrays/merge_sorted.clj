(ns clojure-algorithms.data-structures.arrays.merge-sorted
  "Port of data_structures/arrays/merge_sorted.py.
  Merges two sorted collections after validating sortedness.")

(defn- check-sorted [nums]
  (when-not (or (empty? nums)
                (every? (fn [[a b]] (<= (compare a b) 0))
                        (map vector nums (rest nums))))
    (throw (ex-info (str "nums = " (pr-str (vec nums)) " is not sorted")
                    {:type :value-error}))))

(defn merge-sorted-arrays
  "Merge sorted NUMS1 and NUMS2 into one sorted vector."
  [nums1 nums2]
  (check-sorted nums1)
  (check-sorted nums2)
  (loop [a (seq nums1) b (seq nums2) out []]
    (cond
      (empty? a) (into out b)
      (empty? b) (into out a)
      (<= (compare (first a) (first b)) 0)
      (recur (next a) b (conj out (first a)))
      :else (recur a (next b) (conj out (first b))))))
