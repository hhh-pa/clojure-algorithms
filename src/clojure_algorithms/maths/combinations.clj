(ns clojure-algorithms.maths.combinations
  "https://en.wikipedia.org/wiki/Combination")

(defn combinations
  "Returns the number of different combinations of k length which can
be made from n values, where n >= k."
  [n k]
  (when (or (< n k) (< k 0))
    (throw (ex-info "Please enter positive integers for n and k where n >= k"
                    {:type :ValueError})))
  (reduce (fn [res i] (quot (*' res (- n i)) (inc i)))
          1
          (range k)))
