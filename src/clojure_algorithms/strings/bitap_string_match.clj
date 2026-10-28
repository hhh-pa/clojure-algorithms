(ns clojure-algorithms.strings.bitap-string-match
  "Bitap exact string matching for lowercase text/pattern.
  Ported from TheAlgorithms/Python."
  (:import [java.math BigInteger]))

(def ^:private one BigInteger/ONE)

(defn bitap-string-match
  "Index of the first occurrence of `pattern` in `text`, or -1."
  [text pattern]
  (if (empty? pattern)
    0
    (let [m (count pattern)]
      (if (> m (count text))
        -1
        (let [all-ones (.not BigInteger/ZERO)
              mask (reduce (fn [ms [i ch]]
                             (update ms (- (int ch) (int \a))
                                     (fn [w]
                                       (.and ^BigInteger w
                                             (.not (.shiftLeft one i))))))
                           (vec (repeat 27 all-ones))
                           (map-indexed vector pattern))]
          (loop [i 0 ^BigInteger state (.not one)]
            (if (>= i (count text))
              -1
              (let [ti (- (int (nth text i)) (int \a))
                    state2 (.shiftLeft
                            (if (and (<= 0 ti) (< ti 27))
                              (.or state ^BigInteger (nth mask ti))
                              all-ones)
                            1)]
                (if (.equals (.and state2 (.shiftLeft one m))
                             BigInteger/ZERO)
                  (- i m -1)
                  (recur (inc i) state2))))))))))
