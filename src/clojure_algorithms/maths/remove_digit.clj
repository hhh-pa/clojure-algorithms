(ns clojure-algorithms.maths.remove-digit
  "Port of maths/remove_digit.py")

(defn remove-digit
  "Returns the biggest possible result that can be achieved by removing
exactly one digit from the given number."
  [num]
  (when-not (integer? num)
    (throw (ex-info "only integers accepted as input" {:type :TypeError})))
  (let [digits (str (if (neg? num) (-' num) num))]
    (apply max
           (for [i (range (count digits))]
             (let [candidate (str (subs digits 0 i) (subs digits (inc i)))]
               (if (empty? candidate)
                 (throw (ex-info "invalid literal for int() with base 10: ''"
                                 {:type :ValueError}))
                 (bigint candidate)))))))
