(ns clojure-algorithms.maths.average-median
  "Find the median of a list of numbers.
  Wiki: https://en.wikipedia.org/wiki/Median")

(defn median
  "Returns the median of a collection of numbers."
  [nums]
  (let [sorted-nums (sort nums)
        length (count sorted-nums)
        mid-index (quot length 2)]
    (if (even? length)
      (/ (double (+ (nth sorted-nums mid-index)
                    (nth sorted-nums (dec mid-index))))
         2)
      (nth sorted-nums mid-index))))

(defn main
  [& _args]
  ;; Upstream main only runs the module doctests; there is no portable
  ;; pure-Clojure behavior, so it is kept as a stub.
  (throw (ex-info "main runs the doctest suite in Python; no port applicable"
                  {:type :UnsupportedOperation})))
