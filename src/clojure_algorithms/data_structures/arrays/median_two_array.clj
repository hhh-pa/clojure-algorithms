(ns clojure-algorithms.data-structures.arrays.median-two-array
  "Port of data_structures/arrays/median_two_array.py.
  Median of two sorted arrays.")

(defn find-median-sorted-arrays
  "Median of NUMS1 and NUMS2 as a double."
  [nums1 nums2]
  (when (and (empty? nums1) (empty? nums2))
    (throw (ex-info "Both input arrays are empty." {:type :value-error})))
  (let [merged (vec (sort (concat nums1 nums2)))
        total (count merged)]
    (if (odd? total)
      (double (nth merged (quot total 2)))
      (/ (+ (double (nth merged (dec (quot total 2))))
            (double (nth merged (quot total 2))))
         2.0))))
