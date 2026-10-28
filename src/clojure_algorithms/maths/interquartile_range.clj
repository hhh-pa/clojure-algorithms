(ns clojure-algorithms.maths.interquartile-range
  "An implementation of interquartile range (IQR) which is a measure of statistical
dispersion, which is the spread of the data.

The function takes the list of numeric values as input and returns the IQR.

Script inspired by this Wikipedia article:
https://en.wikipedia.org/wiki/Interquartile_range")

(defn find-median
  "Median of nums. For an even count, the average of the two middle values
(as a double); for an odd count, the middle value as-is."
  [nums]
  (let [v (vec nums)
        div (quot (count v) 2)]
    (if (odd? (count v))
      (nth v div)
      (/ (+ (double (nth v div)) (double (nth v (dec div)))) 2.0))))

(defn interquartile-range
  "Return the interquartile range (q3 - q1) for a list of numeric values."
  [nums]
  (when (empty? nums)
    (throw (ex-info "The list is empty. Provide a non-empty list."
                    {:type :ValueError})))
  (let [sorted (sort nums)
        length (count sorted)
        div (quot length 2)
        md (mod length 2)
        q1 (find-median (take div sorted))
        q3 (find-median (drop (+ div md) sorted))]
    (- q3 q1)))
