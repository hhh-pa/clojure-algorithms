(ns clojure-algorithms.maths.special-numbers.hamming-numbers
  "A Hamming number is a positive integer of the form 2^i*3^j*5^k, for some
non-negative integers i, j, and k. They are often referred to as regular numbers.
More info at: https://en.wikipedia.org/wiki/Regular_number.")

(defn- advance
  "Smallest index at or after `idx` whose multiple by `mult` exceeds `lastv`."
  [lst idx mult lastv]
  (loop [i idx]
    (if (<= (* (nth lst i) mult) lastv)
      (recur (inc i))
      i)))

(defn hamming
  "Returns a vector holding the first `n-element` Hamming numbers in order.
`n-element` must be a positive integer. Throws an ex-info with :type
:ValueError when it is below 1 (non-integers are truncated toward zero,
mirroring Python's int())."
  [n-element]
  (let [n (cond
            (integer? n-element) n-element
            (number? n-element) (int n-element)
            :else (throw (ex-info "n_element should be a positive number"
                                  {:type :ValueError})))]
    (when (< n 1)
      (throw (ex-info "n_element should be a positive number"
                      {:type :ValueError})))
    (loop [lst [1] i 0 j 0 k 0]
      (if (>= (count lst) n)
        lst
        (let [lastv (peek lst)
              i (advance lst i 2 lastv)
              j (advance lst j 3 lastv)
              k (advance lst k 5 lastv)]
          (recur (conj lst (min (* (nth lst i) 2)
                                (* (nth lst j) 3)
                                (* (nth lst k) 5)))
                 i j k))))))
