(ns clojure-algorithms.hashes.luhn
  "Luhn checksum validation, as used for credit-card numbers.")

(defn is-luhn
  "Returns true when the digit string passes Luhn validation."
  [string]
  (let [digits (mapv #(Character/digit ^char % 10) string)
        check (peek digits)
        body (rseq (pop digits))
        total (reduce (fn [sum [i d]]
                        (if (even? i)
                          (let [doubled (* 2 d)]
                            (+ sum (if (> doubled 9) (- doubled 9) doubled)))
                          (+ sum d)))
                      check
                      (map-indexed vector body))]
    (zero? (mod total 10))))
