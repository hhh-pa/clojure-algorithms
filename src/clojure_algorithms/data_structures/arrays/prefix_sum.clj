(ns clojure-algorithms.data-structures.arrays.prefix-sum
  "Port of data_structures/arrays/prefix_sum.py.
  Prefix sums with range-sum and sub-array-sum queries.")

(defn make-prefix-sum
  "Build a prefix-sum structure over ARR."
  [arr]
  {:arr (vec arr) :prefix (vec (reductions + arr))})

(defn get-sum
  "Sum of ARR between indexes L and R inclusive."
  [ps l r]
  (let [{:keys [arr prefix]} ps
        n (count arr)]
    (when (empty? arr)
      (throw (ex-info "The array is empty." {:type :value-error})))
    (when-not (<= 0 l r (dec n))
      (throw (ex-info "Invalid range specified." {:type :value-error})))
    (- (nth prefix r) (if (pos? l) (nth prefix (dec l)) 0))))

(defn contains-sum
  "True when some contiguous sub-array of ARR sums to TARGET-SUM."
  [ps target-sum]
  (loop [sums #{0} items (:prefix ps)]
    (if (empty? items)
      false
      (let [s (first items)]
        (if (contains? sums (- s target-sum))
          true
          (recur (conj sums s) (rest items)))))))
