(ns clojure-algorithms.maths.pell-number
  "Port of maths/pell_number.py")

(defn- check-subscript
  "Validate the subscript shared by both Pell functions."
  [subscript]
  (when-not (integer? subscript)
    (throw (ex-info "The input must be an integer." {:type :ValueError})))
  (when (< subscript 0)
    (throw (ex-info "The input number must be non-negative."
                    {:type :ValueError}))))

(defn pell-number-iterative
  "Return the subscript-th Pell number, computed iteratively.
  Pell numbers follow P_0 = 0, P_1 = 1, P_n = 2 * P_(n-1) + P_(n-2).
  https://en.wikipedia.org/wiki/Pell_number"
  [subscript]
  (check-subscript subscript)
  (if (<= subscript 1)
    subscript
    (loop [i 2
           prev-prev-num 0
           prev-num 1]
      (if (> i subscript)
        prev-num
        (recur (inc i)
               prev-num
               (+' (*' 2 prev-num) prev-prev-num))))))

(defn pell-number-recursive
  "Return the subscript-th Pell number, computed recursively.
  Grows exponentially with subscript; prefer pell-number-iterative
  for large values."
  [subscript]
  (check-subscript subscript)
  (if (<= subscript 1)
    subscript
    (+' (*' 2 (pell-number-recursive (dec subscript)))
        (pell-number-recursive (- subscript 2)))))
